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

class CobolNistIFTest : RewriteTest {

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
    fun if1014_2() = rewriteRun(
        cobol("""
            000100 IDENTIFICATION DIVISION.                                         IF1014.2
            000200 PROGRAM-ID.                                                      IF1014.2
            000300     IF101A.                                                      IF1014.2
            000400                                                                  IF1014.2
            000500***********************************************************       IF1014.2
            000600*                                                         *       IF1014.2
            000700*   This program is intended to form part of the CCVS85   *       IF1014.2
            000800*   COBOL Test Suite. It contains tests for the           *       IF1014.2
            000900*   Intrinsic Function ACOS.                              *       IF1014.2
            001000*                                                         *       IF1014.2
            001100***********************************************************       IF1014.2
            001200 ENVIRONMENT DIVISION.                                            IF1014.2
            001300 CONFIGURATION SECTION.                                           IF1014.2
            001400 SOURCE-COMPUTER.                                                 IF1014.2
            001500     XXXXX082.                                                    IF1014.2
            001600 OBJECT-COMPUTER.                                                 IF1014.2
            001700     XXXXX083.                                                    IF1014.2
            001800 INPUT-OUTPUT SECTION.                                            IF1014.2
            001900 FILE-CONTROL.                                                    IF1014.2
            002000     SELECT PRINT-FILE ASSIGN TO                                  IF1014.2
            002100     XXXXX055.                                                    IF1014.2
            002200 DATA DIVISION.                                                   IF1014.2
            002300 FILE SECTION.                                                    IF1014.2
            002400 FD  PRINT-FILE.                                                  IF1014.2
            002500 01  PRINT-REC PICTURE X(120).                                    IF1014.2
            002600 01  DUMMY-RECORD PICTURE X(120).                                 IF1014.2
            002700 WORKING-STORAGE SECTION.                                         IF1014.2
            002800***********************************************************       IF1014.2
            002900* Variables specific to the Intrinsic Function Test IF101A*       IF1014.2
            003000***********************************************************       IF1014.2
            003100 01  A                   PIC S9(5)V9(5)      VALUE -0.00004.      IF1014.2
            003200 01  B                   PIC S9(10)          VALUE 4.             IF1014.2
            003300 01  C                   PIC S9(10)          VALUE 100000.        IF1014.2
            003400 01  D                   PIC S9(10)          VALUE 1000.          IF1014.2
            003500 01  PI                  PIC S9V9(17)        VALUE 3.141592654.   IF1014.2
            003600 01  ARG1                PIC S9V9(17)        VALUE 0.00.          IF1014.2
            003700 01  SQRT2               PIC S9V9(17)        VALUE 1.414213562.   IF1014.2
            003800 01  SQRT3D2             PIC S9V9(17)        VALUE 0.866025403.   IF1014.2
            003900 01  ARR                                     VALUE "40537".       IF1014.2
            004000     02  IND OCCURS 5 TIMES PIC 9.                                IF1014.2
            004100 01  TEMP                PIC S9(5)V9(5).                          IF1014.2
            004200 01  WS-NUM              PIC S9(5)V9(6).                          IF1014.2
            004300 01  MIN-RANGE           PIC S9(5)V9(7).                          IF1014.2
            004400 01  MAX-RANGE           PIC S9(5)V9(7).                          IF1014.2
            004500*                                                                 IF1014.2
            004600**********************************************************        IF1014.2
            004700*                                                                 IF1014.2
            004800 01  TEST-RESULTS.                                                IF1014.2
            004900     02 FILLER                   PIC X      VALUE SPACE.          IF1014.2
            005000     02 FEATURE                  PIC X(20)  VALUE SPACE.          IF1014.2
            005100     02 FILLER                   PIC X      VALUE SPACE.          IF1014.2
            005200     02 P-OR-F                   PIC X(5)   VALUE SPACE.          IF1014.2
            005300     02 FILLER                   PIC X      VALUE SPACE.          IF1014.2
            005400     02  PAR-NAME.                                                IF1014.2
            005500       03 FILLER                 PIC X(19)  VALUE SPACE.          IF1014.2
            005600       03  PARDOT-X              PIC X      VALUE SPACE.          IF1014.2
            005700       03 DOTVALUE               PIC 99     VALUE ZERO.           IF1014.2
            005800     02 FILLER                   PIC X(8)   VALUE SPACE.          IF1014.2
            005900     02 RE-MARK                  PIC X(61).                       IF1014.2
            006000 01  TEST-COMPUTED.                                               IF1014.2
            006100     02 FILLER                   PIC X(30)  VALUE SPACE.          IF1014.2
            006200     02 FILLER                   PIC X(17)  VALUE                 IF1014.2
            006300            "       COMPUTED=".                                   IF1014.2
            006400     02 COMPUTED-X.                                               IF1014.2
            006500     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          IF1014.2
            006600     03 COMPUTED-N               REDEFINES COMPUTED-A             IF1014.2
            006700                                 PIC -9(9).9(9).                  IF1014.2
            006800     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         IF1014.2
            006900     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     IF1014.2
            007000     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     IF1014.2
            007100     03       CM-18V0 REDEFINES COMPUTED-A.                       IF1014.2
            007200         04 COMPUTED-18V0                    PIC -9(18).          IF1014.2
            007300         04 FILLER                           PIC X.               IF1014.2
            007400     03 FILLER PIC X(50) VALUE SPACE.                             IF1014.2
            007500 01  TEST-CORRECT.                                                IF1014.2
            007600     02 FILLER PIC X(30) VALUE SPACE.                             IF1014.2
            007700     02 FILLER PIC X(17) VALUE "       CORRECT =".                IF1014.2
            007800     02 CORRECT-X.                                                IF1014.2
            007900     03 CORRECT-A                  PIC X(20) VALUE SPACE.         IF1014.2
            008000     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      IF1014.2
            008100     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         IF1014.2
            008200     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     IF1014.2
            008300     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     IF1014.2
            008400     03      CR-18V0 REDEFINES CORRECT-A.                         IF1014.2
            008500         04 CORRECT-18V0                     PIC -9(18).          IF1014.2
            008600         04 FILLER                           PIC X.               IF1014.2
            008700     03 FILLER PIC X(2) VALUE SPACE.                              IF1014.2
            008800     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     IF1014.2
            008900 01  TEST-CORRECT-MIN.                                            IF1014.2
            009000     02 FILLER PIC X(30) VALUE SPACE.                             IF1014.2
            009100     02 FILLER PIC X(17) VALUE "     MIN VALUE =".                IF1014.2
            009200     02 CORRECTMI-X.                                              IF1014.2
            009300     03 CORRECTMI-A                 PIC X(20) VALUE SPACE.        IF1014.2
            009400     03 CORRECT-MIN    REDEFINES CORRECTMI-A     PIC -9(9).9(9).  IF1014.2
            009500     03 CORRECTMI-0V18 REDEFINES CORRECTMI-A     PIC -.9(18).     IF1014.2
            009600     03 CORRECTMI-4V14 REDEFINES CORRECTMI-A     PIC -9(4).9(14). IF1014.2
            009700     03 CORRECTMI-14V4 REDEFINES CORRECTMI-A     PIC -9(14).9(4). IF1014.2
            009800     03      CR-18V0 REDEFINES CORRECTMI-A.                       IF1014.2
            009900         04 CORRECTMI-18V0                     PIC -9(18).        IF1014.2
            010000         04 FILLER                           PIC X.               IF1014.2
            010100     03 FILLER PIC X(2) VALUE SPACE.                              IF1014.2
            010200     03 FILLER                           PIC X(48) VALUE SPACE.   IF1014.2
            010300 01  TEST-CORRECT-MAX.                                            IF1014.2
            010400     02 FILLER PIC X(30) VALUE SPACE.                             IF1014.2
            010500     02 FILLER PIC X(17) VALUE "     MAX VALUE =".                IF1014.2
            010600     02 CORRECTMA-X.                                              IF1014.2
            010700     03 CORRECTMA-A                  PIC X(20) VALUE SPACE.       IF1014.2
            010800     03 CORRECT-MAX    REDEFINES CORRECTMA-A     PIC -9(9).9(9).  IF1014.2
            010900     03 CORRECTMA-0V18 REDEFINES CORRECTMA-A     PIC -.9(18).     IF1014.2
            011000     03 CORRECTMA-4V14 REDEFINES CORRECTMA-A     PIC -9(4).9(14). IF1014.2
            011100     03 CORRECTMA-14V4 REDEFINES CORRECTMA-A     PIC -9(14).9(4). IF1014.2
            011200     03      CR-18V0 REDEFINES CORRECTMA-A.                       IF1014.2
            011300         04 CORRECTMA-18V0                     PIC -9(18).        IF1014.2
            011400         04 FILLER                           PIC X.               IF1014.2
            011500     03 FILLER PIC X(2) VALUE SPACE.                              IF1014.2
            011600     03 CORMA-ANSI-REFERENCE             PIC X(48) VALUE SPACE.   IF1014.2
            011700 01  CCVS-C-1.                                                    IF1014.2
            011800     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PAIF1014.2
            011900-    "SS  PARAGRAPH-NAME                                          IF1014.2
            012000-    "       REMARKS".                                            IF1014.2
            012100     02 FILLER                     PIC X(20)    VALUE SPACE.      IF1014.2
            012200 01  CCVS-C-2.                                                    IF1014.2
            012300     02 FILLER                     PIC X        VALUE SPACE.      IF1014.2
            012400     02 FILLER                     PIC X(6)     VALUE "TESTED".   IF1014.2
            012500     02 FILLER                     PIC X(15)    VALUE SPACE.      IF1014.2
            012600     02 FILLER                     PIC X(4)     VALUE "FAIL".     IF1014.2
            012700     02 FILLER                     PIC X(94)    VALUE SPACE.      IF1014.2
            012800 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       IF1014.2
            012900 01  REC-CT                        PIC 99       VALUE ZERO.       IF1014.2
            013000 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       IF1014.2
            013100 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       IF1014.2
            013200 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       IF1014.2
            013300 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       IF1014.2
            013400 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       IF1014.2
            013500 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       IF1014.2
            013600 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      IF1014.2
            013700 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       IF1014.2
            013800 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     IF1014.2
            013900 01  CCVS-H-1.                                                    IF1014.2
            014000     02  FILLER                    PIC X(39)    VALUE SPACES.     IF1014.2
            014100     02  FILLER                    PIC X(42)    VALUE             IF1014.2
            014200     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 IF1014.2
            014300     02  FILLER                    PIC X(39)    VALUE SPACES.     IF1014.2
            014400 01  CCVS-H-2A.                                                   IF1014.2
            014500   02  FILLER                        PIC X(40)  VALUE SPACE.      IF1014.2
            014600   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  IF1014.2
            014700   02  FILLER                        PIC XXXX   VALUE             IF1014.2
            014800     "4.2 ".                                                      IF1014.2
            014900   02  FILLER                        PIC X(28)  VALUE             IF1014.2
            015000            " COPY - NOT FOR DISTRIBUTION".                       IF1014.2
            015100   02  FILLER                        PIC X(41)  VALUE SPACE.      IF1014.2
            015200                                                                  IF1014.2
            015300 01  CCVS-H-2B.                                                   IF1014.2
            015400   02  FILLER                        PIC X(15)  VALUE             IF1014.2
            015500            "TEST RESULT OF ".                                    IF1014.2
            015600   02  TEST-ID                       PIC X(9).                    IF1014.2
            015700   02  FILLER                        PIC X(4)   VALUE             IF1014.2
            015800            " IN ".                                               IF1014.2
            015900   02  FILLER                        PIC X(12)  VALUE             IF1014.2
            016000     " HIGH       ".                                              IF1014.2
            016100   02  FILLER                        PIC X(22)  VALUE             IF1014.2
            016200            " LEVEL VALIDATION FOR ".                             IF1014.2
            016300   02  FILLER                        PIC X(58)  VALUE             IF1014.2
            016400     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".IF1014.2
            016500 01  CCVS-H-3.                                                    IF1014.2
            016600     02  FILLER                      PIC X(34)  VALUE             IF1014.2
            016700            " FOR OFFICIAL USE ONLY    ".                         IF1014.2
            016800     02  FILLER                      PIC X(58)  VALUE             IF1014.2
            016900     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".IF1014.2
            017000     02  FILLER                      PIC X(28)  VALUE             IF1014.2
            017100            "  COPYRIGHT   1985 ".                                IF1014.2
            017200 01  CCVS-E-1.                                                    IF1014.2
            017300     02 FILLER                       PIC X(52)  VALUE SPACE.      IF1014.2
            017400     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              IF1014.2
            017500     02 ID-AGAIN                     PIC X(9).                    IF1014.2
            017600     02 FILLER                       PIC X(45)  VALUE SPACES.     IF1014.2
            017700 01  CCVS-E-2.                                                    IF1014.2
            017800     02  FILLER                      PIC X(31)  VALUE SPACE.      IF1014.2
            017900     02  FILLER                      PIC X(21)  VALUE SPACE.      IF1014.2
            018000     02 CCVS-E-2-2.                                               IF1014.2
            018100         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      IF1014.2
            018200         03 FILLER                   PIC X      VALUE SPACE.      IF1014.2
            018300         03 ENDER-DESC               PIC X(44)  VALUE             IF1014.2
            018400            "ERRORS ENCOUNTERED".                                 IF1014.2
            018500 01  CCVS-E-3.                                                    IF1014.2
            018600     02  FILLER                      PIC X(22)  VALUE             IF1014.2
            018700            " FOR OFFICIAL USE ONLY".                             IF1014.2
            018800     02  FILLER                      PIC X(12)  VALUE SPACE.      IF1014.2
            018900     02  FILLER                      PIC X(58)  VALUE             IF1014.2
            019000     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".IF1014.2
            019100     02  FILLER                      PIC X(13)  VALUE SPACE.      IF1014.2
            019200     02 FILLER                       PIC X(15)  VALUE             IF1014.2
            019300             " COPYRIGHT 1985".                                   IF1014.2
            019400 01  CCVS-E-4.                                                    IF1014.2
            019500     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      IF1014.2
            019600     02 FILLER                       PIC X(4)   VALUE " OF ".     IF1014.2
            019700     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      IF1014.2
            019800     02 FILLER                       PIC X(40)  VALUE             IF1014.2
            019900      "  TESTS WERE EXECUTED SUCCESSFULLY".                       IF1014.2
            020000 01  XXINFO.                                                      IF1014.2
            020100     02 FILLER                       PIC X(19)  VALUE             IF1014.2
            020200            "*** INFORMATION ***".                                IF1014.2
            020300     02 INFO-TEXT.                                                IF1014.2
            020400       04 FILLER                     PIC X(8)   VALUE SPACE.      IF1014.2
            020500       04 XXCOMPUTED                 PIC X(20).                   IF1014.2
            020600       04 FILLER                     PIC X(5)   VALUE SPACE.      IF1014.2
            020700       04 XXCORRECT                  PIC X(20).                   IF1014.2
            020800     02 INF-ANSI-REFERENCE           PIC X(48).                   IF1014.2
            020900 01  HYPHEN-LINE.                                                 IF1014.2
            021000     02 FILLER  PIC IS X VALUE IS SPACE.                          IF1014.2
            021100     02 FILLER  PIC IS X(65)    VALUE IS "************************IF1014.2
            021200-    "*****************************************".                 IF1014.2
            021300     02 FILLER  PIC IS X(54)    VALUE IS "************************IF1014.2
            021400-    "******************************".                            IF1014.2
            021500 01  CCVS-PGM-ID                     PIC X(9)   VALUE             IF1014.2
            021600     "IF101A".                                                    IF1014.2
            021700 PROCEDURE DIVISION.                                              IF1014.2
            021800 CCVS1 SECTION.                                                   IF1014.2
            021900 OPEN-FILES.                                                      IF1014.2
            022000     OPEN     OUTPUT PRINT-FILE.                                  IF1014.2
            022100     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   IF1014.2
            022200     MOVE    SPACE TO TEST-RESULTS.                               IF1014.2
            022300     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             IF1014.2
            022400     GO TO CCVS1-EXIT.                                            IF1014.2
            022500 CLOSE-FILES.                                                     IF1014.2
            022600     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   IF1014.2
            022700 TERMINATE-CCVS.                                                  IF1014.2
            022800     STOP     RUN.                                                IF1014.2
            022900 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         IF1014.2
            023000 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           IF1014.2
            023100 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          IF1014.2
            023200 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      IF1014.2
            023300     MOVE "****TEST DELETED****" TO RE-MARK.                      IF1014.2
            023400 PRINT-DETAIL.                                                    IF1014.2
            023500     IF REC-CT NOT EQUAL TO ZERO                                  IF1014.2
            023600             MOVE "." TO PARDOT-X                                 IF1014.2
            023700             MOVE REC-CT TO DOTVALUE.                             IF1014.2
            023800     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      IF1014.2
            023900     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               IF1014.2
            024000        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 IF1014.2
            024100          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 IF1014.2
            024200     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              IF1014.2
            024300     MOVE SPACE TO CORRECT-X.                                     IF1014.2
            024400     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         IF1014.2
            024500     MOVE     SPACE TO RE-MARK.                                   IF1014.2
            024600 HEAD-ROUTINE.                                                    IF1014.2
            024700     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  IF1014.2
            024800     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  IF1014.2
            024900     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  IF1014.2
            025000     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  IF1014.2
            025100 COLUMN-NAMES-ROUTINE.                                            IF1014.2
            025200     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           IF1014.2
            025300     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1014.2
            025400     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        IF1014.2
            025500 END-ROUTINE.                                                     IF1014.2
            025600     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.IF1014.2
            025700 END-RTN-EXIT.                                                    IF1014.2
            025800     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1014.2
            025900 END-ROUTINE-1.                                                   IF1014.2
            026000      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      IF1014.2
            026100      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               IF1014.2
            026200      ADD PASS-COUNTER TO ERROR-HOLD.                             IF1014.2
            026300      MOVE PASS-COUNTER TO CCVS-E-4-1.                            IF1014.2
            026400      MOVE ERROR-HOLD TO CCVS-E-4-2.                              IF1014.2
            026500      MOVE CCVS-E-4 TO CCVS-E-2-2.                                IF1014.2
            026600      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           IF1014.2
            026700  END-ROUTINE-12.                                                 IF1014.2
            026800      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        IF1014.2
            026900     IF       ERROR-COUNTER IS EQUAL TO ZERO                      IF1014.2
            027000         MOVE "NO " TO ERROR-TOTAL                                IF1014.2
            027100         ELSE                                                     IF1014.2
            027200         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       IF1014.2
            027300     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           IF1014.2
            027400     PERFORM WRITE-LINE.                                          IF1014.2
            027500 END-ROUTINE-13.                                                  IF1014.2
            027600     IF DELETE-COUNTER IS EQUAL TO ZERO                           IF1014.2
            027700         MOVE "NO " TO ERROR-TOTAL  ELSE                          IF1014.2
            027800         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      IF1014.2
            027900     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   IF1014.2
            028000     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           IF1014.2
            028100      IF   INSPECT-COUNTER EQUAL TO ZERO                          IF1014.2
            028200          MOVE "NO " TO ERROR-TOTAL                               IF1014.2
            028300      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   IF1014.2
            028400      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            IF1014.2
            028500      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          IF1014.2
            028600     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           IF1014.2
            028700 WRITE-LINE.                                                      IF1014.2
            028800     ADD 1 TO RECORD-COUNT.                                       IF1014.2
            028900*    IF RECORD-COUNT GREATER 42                                   IF1014.2
            029000*        MOVE DUMMY-RECORD TO DUMMY-HOLD                          IF1014.2
            029100*        MOVE SPACE TO DUMMY-RECORD                               IF1014.2
            029200*        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  IF1014.2
            029300*        MOVE CCVS-H-1  TO DUMMY-RECORD  PERFORM WRT-LN 2 TIMES   IF1014.2
            029400*        MOVE CCVS-H-2A TO DUMMY-RECORD  PERFORM WRT-LN 2 TIMES   IF1014.2
            029500*        MOVE CCVS-H-2B TO DUMMY-RECORD  PERFORM WRT-LN 3 TIMES   IF1014.2
            029600*        MOVE CCVS-H-3  TO DUMMY-RECORD  PERFORM WRT-LN 3 TIMES   IF1014.2
            029700*        MOVE CCVS-C-1  TO DUMMY-RECORD  PERFORM WRT-LN           IF1014.2
            029800*        MOVE CCVS-C-2  TO DUMMY-RECORD  PERFORM WRT-LN           IF1014.2
            029900*        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          IF1014.2
            030000*        MOVE DUMMY-HOLD TO DUMMY-RECORD                          IF1014.2
            030100*        MOVE ZERO TO RECORD-COUNT.                               IF1014.2
            030200     PERFORM WRT-LN.                                              IF1014.2
            030300 WRT-LN.                                                          IF1014.2
            030400     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               IF1014.2
            030500     MOVE SPACE TO DUMMY-RECORD.                                  IF1014.2
            030600 BLANK-LINE-PRINT.                                                IF1014.2
            030700     PERFORM WRT-LN.                                              IF1014.2
            030800 FAIL-ROUTINE.                                                    IF1014.2
            030900     IF     COMPUTED-X NOT EQUAL TO SPACE                         IF1014.2
            031000            GO TO FAIL-ROUTINE-WRITE.                             IF1014.2
            031100     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.IF1014.2
            031200     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 IF1014.2
            031300     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   IF1014.2
            031400     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1014.2
            031500     MOVE   SPACES TO INF-ANSI-REFERENCE.                         IF1014.2
            031600     GO TO  FAIL-ROUTINE-EX.                                      IF1014.2
            031700 FAIL-ROUTINE-WRITE.                                              IF1014.2
            031800     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE.        IF1014.2
            031900     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE                  IF1014.2
            032000                              CORMA-ANSI-REFERENCE.               IF1014.2
            032100     IF CORRECT-MIN NOT EQUAL TO SPACES THEN                      IF1014.2
            032200           MOVE TEST-CORRECT-MIN TO PRINT-REC PERFORM WRITE-LINE  IF1014.2
            032300           MOVE TEST-CORRECT-MAX TO PRINT-REC PERFORM WRITE-LINE  IF1014.2
            032400     ELSE                                                         IF1014.2
            032500           MOVE TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE.     IF1014.2
            032600     PERFORM WRITE-LINE.                                          IF1014.2
            032700     MOVE   SPACES TO COR-ANSI-REFERENCE.                         IF1014.2
            032800 FAIL-ROUTINE-EX. EXIT.                                           IF1014.2
            032900 BAIL-OUT.                                                        IF1014.2
            033000     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   IF1014.2
            033100     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           IF1014.2
            033200 BAIL-OUT-WRITE.                                                  IF1014.2
            033300     MOVE CORRECT-A TO XXCORRECT.                                 IF1014.2
            033400     MOVE COMPUTED-A TO XXCOMPUTED.                               IF1014.2
            033500     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 IF1014.2
            033600     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1014.2
            033700     MOVE   SPACES TO INF-ANSI-REFERENCE.                         IF1014.2
            033800 BAIL-OUT-EX. EXIT.                                               IF1014.2
            033900 CCVS1-EXIT.                                                      IF1014.2
            034000     EXIT.                                                        IF1014.2
            034100********************************************************          IF1014.2
            034200*                                                      *          IF1014.2
            034300*    Intrinsic Function Tests         IF101A - ACOS    *          IF1014.2
            034400*                                                      *          IF1014.2
            034500********************************************************          IF1014.2
            034600 SECT-IF101A SECTION.                                             IF1014.2
            034700 F-ACOS-INFO.                                                     IF1014.2
            034800     MOVE     "See ref. A-33 2.5" TO ANSI-REFERENCE.              IF1014.2
            034900     MOVE     "ACOS Function" TO FEATURE.                         IF1014.2
            035000*****************TEST (a) - SIMPLE TEST*****************          IF1014.2
            035100 F-ACOS-01.                                                       IF1014.2
            035200     MOVE ZERO TO WS-NUM.                                         IF1014.2
            035300     MOVE  0.000000 TO MIN-RANGE.                                 IF1014.2
            035400     MOVE  0.000020 TO MAX-RANGE.                                 IF1014.2
            035500 F-ACOS-TEST-01.                                                  IF1014.2
            035600     COMPUTE WS-NUM = FUNCTION ACOS(1.0).                         IF1014.2
            035700     IF (WS-NUM >= MIN-RANGE) AND                                 IF1014.2
            035800        (WS-NUM <= MAX-RANGE) THEN                                IF1014.2
            035900                    PERFORM PASS                                  IF1014.2
            036000     ELSE                                                         IF1014.2
            036100                    MOVE WS-NUM TO COMPUTED-N                     IF1014.2
            036200                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1014.2
            036300                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1014.2
            036400                    PERFORM FAIL.                                 IF1014.2
            036500     GO TO F-ACOS-WRITE-01.                                       IF1014.2
            036600 F-ACOS-DELETE-01.                                                IF1014.2
            036700     PERFORM  DE-LETE.                                            IF1014.2
            036800     GO TO    F-ACOS-WRITE-01.                                    IF1014.2
            036900 F-ACOS-WRITE-01.                                                 IF1014.2
            037000     MOVE "F-ACOS-01" TO PAR-NAME.                                IF1014.2
            037100     PERFORM  PRINT-DETAIL.                                       IF1014.2
            037200*****************TEST (b) - SIMPLE TEST*****************          IF1014.2
            037300 F-ACOS-02.                                                       IF1014.2
            037400     MOVE  1.04718 TO MIN-RANGE.                                  IF1014.2
            037500     MOVE  1.04722 TO MAX-RANGE.                                  IF1014.2
            037600 F-ACOS-TEST-02.                                                  IF1014.2
            037700     IF (FUNCTION ACOS(0.5) >= MIN-RANGE) AND                     IF1014.2
            037800        (FUNCTION ACOS(0.5) <= MAX-RANGE) THEN                    IF1014.2
            037900                    PERFORM PASS                                  IF1014.2
            038000     ELSE                                                         IF1014.2
            038100                    PERFORM FAIL.                                 IF1014.2
            038200     GO TO F-ACOS-WRITE-02.                                       IF1014.2
            038300 F-ACOS-DELETE-02.                                                IF1014.2
            038400     PERFORM  DE-LETE.                                            IF1014.2
            038500     GO TO    F-ACOS-WRITE-02.                                    IF1014.2
            038600 F-ACOS-WRITE-02.                                                 IF1014.2
            038700     MOVE "F-ACOS-02" TO PAR-NAME.                                IF1014.2
            038800     PERFORM  PRINT-DETAIL.                                       IF1014.2
            038900*****************TEST (c) - SIMPLE TEST*****************          IF1014.2
            039000 F-ACOS-03.                                                       IF1014.2
            039100     EVALUATE FUNCTION ACOS(0)                                    IF1014.2
            039200     WHEN 1.57076 THRU 1.57082                                    IF1014.2
            039300                    PERFORM PASS                                  IF1014.2
            039400     WHEN OTHER                                                   IF1014.2
            039500                    PERFORM FAIL.                                 IF1014.2
            039600     GO TO F-ACOS-WRITE-03.                                       IF1014.2
            039700 F-ACOS-DELETE-03.                                                IF1014.2
            039800     PERFORM  DE-LETE.                                            IF1014.2
            039900     GO TO    F-ACOS-WRITE-03.                                    IF1014.2
            040000 F-ACOS-WRITE-03.                                                 IF1014.2
            040100     MOVE "F-ACOS-03" TO PAR-NAME.                                IF1014.2
            040200     PERFORM  PRINT-DETAIL.                                       IF1014.2
            040300*****************TEST (d) - SIMPLE TEST*****************          IF1014.2
            040400 F-ACOS-04.                                                       IF1014.2
            040500     MOVE ZERO TO WS-NUM.                                         IF1014.2
            040600     MOVE  3.14153 TO MIN-RANGE.                                  IF1014.2
            040700     MOVE  3.14165 TO MAX-RANGE.                                  IF1014.2
            040800 F-ACOS-TEST-04.                                                  IF1014.2
            040900     COMPUTE WS-NUM = FUNCTION ACOS(-1).                          IF1014.2
            041000     IF (WS-NUM >= MIN-RANGE) AND                                 IF1014.2
            041100        (WS-NUM <= MAX-RANGE) THEN                                IF1014.2
            041200                    PERFORM PASS                                  IF1014.2
            041300     ELSE                                                         IF1014.2
            041400                    MOVE WS-NUM TO COMPUTED-N                     IF1014.2
            041500                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1014.2
            041600                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1014.2
            041700                    PERFORM FAIL.                                 IF1014.2
            041800     GO TO F-ACOS-WRITE-04.                                       IF1014.2
            041900 F-ACOS-DELETE-04.                                                IF1014.2
            042000     PERFORM  DE-LETE.                                            IF1014.2
            042100     GO TO    F-ACOS-WRITE-04.                                    IF1014.2
            042200 F-ACOS-WRITE-04.                                                 IF1014.2
            042300     MOVE "F-ACOS-04" TO PAR-NAME.                                IF1014.2
            042400     PERFORM  PRINT-DETAIL.                                       IF1014.2
            042500*****************TEST (e) - SIMPLE TEST*****************          IF1014.2
            042600 F-ACOS-05.                                                       IF1014.2
            042700     MOVE ZERO TO WS-NUM.                                         IF1014.2
            042800     MOVE  0.044724 TO MIN-RANGE.                                 IF1014.2
            042900     MOVE  0.044726 TO MAX-RANGE.                                 IF1014.2
            043000 F-ACOS-TEST-05.                                                  IF1014.2
            043100     COMPUTE WS-NUM = FUNCTION ACOS(.999).                        IF1014.2
            043200     IF (WS-NUM >= MIN-RANGE) AND                                 IF1014.2
            043300        (WS-NUM <= MAX-RANGE) THEN                                IF1014.2
            043400                    PERFORM PASS                                  IF1014.2
            043500     ELSE                                                         IF1014.2
            043600                    MOVE WS-NUM TO COMPUTED-N                     IF1014.2
            043700                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1014.2
            043800                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1014.2
            043900                    PERFORM FAIL.                                 IF1014.2
            044000     GO TO F-ACOS-WRITE-05.                                       IF1014.2
            044100 F-ACOS-DELETE-05.                                                IF1014.2
            044200     PERFORM  DE-LETE.                                            IF1014.2
            044300     GO TO    F-ACOS-WRITE-05.                                    IF1014.2
            044400 F-ACOS-WRITE-05.                                                 IF1014.2
            044500     MOVE "F-ACOS-05" TO PAR-NAME.                                IF1014.2
            044600     PERFORM  PRINT-DETAIL.                                       IF1014.2
            044700*****************TEST (f) - SIMPLE TEST*****************          IF1014.2
            044800 F-ACOS-06.                                                       IF1014.2
            044900     MOVE ZERO TO WS-NUM.                                         IF1014.2
            045000     MOVE  1.05868 TO MIN-RANGE.                                  IF1014.2
            045100     MOVE  1.05872 TO MAX-RANGE.                                  IF1014.2
            045200 F-ACOS-TEST-06.                                                  IF1014.2
            045300     COMPUTE WS-NUM = FUNCTION ACOS(.49).                         IF1014.2
            045400     IF (WS-NUM >= MIN-RANGE) AND                                 IF1014.2
            045500        (WS-NUM <= MAX-RANGE) THEN                                IF1014.2
            045600                    PERFORM PASS                                  IF1014.2
            045700     ELSE                                                         IF1014.2
            045800                    MOVE WS-NUM TO COMPUTED-N                     IF1014.2
            045900                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1014.2
            046000                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1014.2
            046100                    PERFORM FAIL.                                 IF1014.2
            046200     GO TO F-ACOS-WRITE-06.                                       IF1014.2
            046300 F-ACOS-DELETE-06.                                                IF1014.2
            046400     PERFORM  DE-LETE.                                            IF1014.2
            046500     GO TO    F-ACOS-WRITE-06.                                    IF1014.2
            046600 F-ACOS-WRITE-06.                                                 IF1014.2
            046700     MOVE "F-ACOS-06" TO PAR-NAME.                                IF1014.2
            046800     PERFORM  PRINT-DETAIL.                                       IF1014.2
            046900*****************TEST (g) - SIMPLE TEST*****************          IF1014.2
            047000 F-ACOS-07.                                                       IF1014.2
            047100     MOVE ZERO TO WS-NUM.                                         IF1014.2
            047200     MOVE  1.56976 TO MIN-RANGE.                                  IF1014.2
            047300     MOVE  1.56982 TO MAX-RANGE.                                  IF1014.2
            047400 F-ACOS-TEST-07.                                                  IF1014.2
            047500     COMPUTE WS-NUM = FUNCTION ACOS(.001).                        IF1014.2
            047600     IF (WS-NUM >= MIN-RANGE) AND                                 IF1014.2
            047700        (WS-NUM <= MAX-RANGE) THEN                                IF1014.2
            047800                    PERFORM PASS                                  IF1014.2
            047900     ELSE                                                         IF1014.2
            048000                    MOVE WS-NUM TO COMPUTED-N                     IF1014.2
            048100                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1014.2
            048200                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1014.2
            048300                    PERFORM FAIL.                                 IF1014.2
            048400     GO TO F-ACOS-WRITE-07.                                       IF1014.2
            048500 F-ACOS-DELETE-07.                                                IF1014.2
            048600     PERFORM  DE-LETE.                                            IF1014.2
            048700     GO TO    F-ACOS-WRITE-07.                                    IF1014.2
            048800 F-ACOS-WRITE-07.                                                 IF1014.2
            048900     MOVE "F-ACOS-07" TO PAR-NAME.                                IF1014.2
            049000     PERFORM  PRINT-DETAIL.                                       IF1014.2
            049100*****************TEST (h) - SIMPLE TEST*****************          IF1014.2
            049200 F-ACOS-08.                                                       IF1014.2
            049300     MOVE ZERO TO WS-NUM.                                         IF1014.2
            049400     MOVE  3.09680 TO MIN-RANGE.                                  IF1014.2
            049500     MOVE  3.09692 TO MAX-RANGE.                                  IF1014.2
            049600 F-ACOS-TEST-08.                                                  IF1014.2
            049700     COMPUTE WS-NUM = FUNCTION ACOS(-.999).                       IF1014.2
            049800     IF (WS-NUM >= MIN-RANGE) AND                                 IF1014.2
            049900        (WS-NUM <= MAX-RANGE) THEN                                IF1014.2
            050000                    PERFORM PASS                                  IF1014.2
            050100     ELSE                                                         IF1014.2
            050200                    MOVE WS-NUM TO COMPUTED-N                     IF1014.2
            050300                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1014.2
            050400                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1014.2
            050500                    PERFORM FAIL.                                 IF1014.2
            050600     GO TO F-ACOS-WRITE-08.                                       IF1014.2
            050700 F-ACOS-DELETE-08.                                                IF1014.2
            050800     PERFORM  DE-LETE.                                            IF1014.2
            050900     GO TO    F-ACOS-WRITE-08.                                    IF1014.2
            051000 F-ACOS-WRITE-08.                                                 IF1014.2
            051100     MOVE "F-ACOS-08" TO PAR-NAME.                                IF1014.2
            051200     PERFORM  PRINT-DETAIL.                                       IF1014.2
            051300*****************TEST (i) - SIMPLE TEST*****************          IF1014.2
            051400 F-ACOS-09.                                                       IF1014.2
            051500     MOVE ZERO TO WS-NUM.                                         IF1014.2
            051600     MOVE  1.57080 TO MIN-RANGE.                                  IF1014.2
            051700     MOVE  1.57086 TO MAX-RANGE.                                  IF1014.2
            051800 F-ACOS-TEST-09.                                                  IF1014.2
            051900     COMPUTE WS-NUM = FUNCTION ACOS(A).                           IF1014.2
            052000     IF (WS-NUM >= MIN-RANGE) AND                                 IF1014.2
            052100        (WS-NUM <= MAX-RANGE) THEN                                IF1014.2
            052200                    PERFORM PASS                                  IF1014.2
            052300     ELSE                                                         IF1014.2
            052400                    MOVE WS-NUM TO COMPUTED-N                     IF1014.2
            052500                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1014.2
            052600                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1014.2
            052700                    PERFORM FAIL.                                 IF1014.2
            052800     GO TO F-ACOS-WRITE-09.                                       IF1014.2
            052900 F-ACOS-DELETE-09.                                                IF1014.2
            053000     PERFORM  DE-LETE.                                            IF1014.2
            053100     GO TO    F-ACOS-WRITE-09.                                    IF1014.2
            053200 F-ACOS-WRITE-09.                                                 IF1014.2
            053300     MOVE "F-ACOS-09" TO PAR-NAME.                                IF1014.2
            053400     PERFORM  PRINT-DETAIL.                                       IF1014.2
            053500*****************TEST (j) - SIMPLE TEST*****************          IF1014.2
            053600 F-ACOS-10.                                                       IF1014.2
            053700     MOVE ZERO TO WS-NUM.                                         IF1014.2
            053800     MOVE  1.57074 TO MIN-RANGE.                                  IF1014.2
            053900     MOVE  1.57080 TO MAX-RANGE.                                  IF1014.2
            054000 F-ACOS-TEST-10.                                                  IF1014.2
            054100     COMPUTE WS-NUM = FUNCTION ACOS(.00002).                      IF1014.2
            054200     IF (WS-NUM >= MIN-RANGE) AND                                 IF1014.2
            054300        (WS-NUM <= MAX-RANGE) THEN                                IF1014.2
            054400                    PERFORM PASS                                  IF1014.2
            054500     ELSE                                                         IF1014.2
            054600                    MOVE WS-NUM TO COMPUTED-N                     IF1014.2
            054700                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1014.2
            054800                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1014.2
            054900                    PERFORM FAIL.                                 IF1014.2
            055000     GO TO F-ACOS-WRITE-10.                                       IF1014.2
            055100 F-ACOS-DELETE-10.                                                IF1014.2
            055200     PERFORM  DE-LETE.                                            IF1014.2
            055300     GO TO    F-ACOS-WRITE-10.                                    IF1014.2
            055400 F-ACOS-WRITE-10.                                                 IF1014.2
            055500     MOVE "F-ACOS-10" TO PAR-NAME.                                IF1014.2
            055600     PERFORM  PRINT-DETAIL.                                       IF1014.2
            055700*****************TEST (a) - COMPLEX TEST****************          IF1014.2
            055800 F-ACOS-11.                                                       IF1014.2
            055900     MOVE ZERO TO WS-NUM.                                         IF1014.2
            056000     MOVE  0.785367 TO MIN-RANGE.                                 IF1014.2
            056100     MOVE  0.785429 TO MAX-RANGE.                                 IF1014.2
            056200 F-ACOS-TEST-11.                                                  IF1014.2
            056300     COMPUTE WS-NUM = FUNCTION ACOS(1 / SQRT2).                   IF1014.2
            056400     IF (WS-NUM >= MIN-RANGE) AND                                 IF1014.2
            056500        (WS-NUM <= MAX-RANGE) THEN                                IF1014.2
            056600                    PERFORM PASS                                  IF1014.2
            056700     ELSE                                                         IF1014.2
            056800                    MOVE WS-NUM TO COMPUTED-N                     IF1014.2
            056900                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1014.2
            057000                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1014.2
            057100                    PERFORM FAIL.                                 IF1014.2
            057200     GO TO F-ACOS-WRITE-11.                                       IF1014.2
            057300 F-ACOS-DELETE-11.                                                IF1014.2
            057400     PERFORM  DE-LETE.                                            IF1014.2
            057500     GO TO    F-ACOS-WRITE-11.                                    IF1014.2
            057600 F-ACOS-WRITE-11.                                                 IF1014.2
            057700     MOVE "F-ACOS-11" TO PAR-NAME.                                IF1014.2
            057800     PERFORM  PRINT-DETAIL.                                       IF1014.2
            057900*****************TEST (b) - COMPLEX TEST****************          IF1014.2
            058000 F-ACOS-12.                                                       IF1014.2
            058100     MOVE ZERO TO WS-NUM.                                         IF1014.2
            058200     MOVE  0.523577  TO MIN-RANGE.                                IF1014.2
            058300     MOVE  0.523619 TO MAX-RANGE.                                 IF1014.2
            058400 F-ACOS-TEST-12.                                                  IF1014.2
            058500     COMPUTE WS-NUM = FUNCTION ACOS(SQRT3D2).                     IF1014.2
            058600     IF (WS-NUM >= MIN-RANGE) AND                                 IF1014.2
            058700        (WS-NUM <= MAX-RANGE) THEN                                IF1014.2
            058800                    PERFORM PASS                                  IF1014.2
            058900     ELSE                                                         IF1014.2
            059000                    MOVE WS-NUM TO COMPUTED-N                     IF1014.2
            059100                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1014.2
            059200                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1014.2
            059300                    PERFORM FAIL.                                 IF1014.2
            059400     GO TO F-ACOS-WRITE-12.                                       IF1014.2
            059500 F-ACOS-DELETE-12.                                                IF1014.2
            059600     PERFORM  DE-LETE.                                            IF1014.2
            059700     GO TO    F-ACOS-WRITE-12.                                    IF1014.2
            059800 F-ACOS-WRITE-12.                                                 IF1014.2
            059900     MOVE "F-ACOS-12" TO PAR-NAME.                                IF1014.2
            060000     PERFORM  PRINT-DETAIL.                                       IF1014.2
            060100*****************TEST (c) - COMPLEX TEST****************          IF1014.2
            060200 F-ACOS-13.                                                       IF1014.2
            060300     MOVE ZERO TO WS-NUM.                                         IF1014.2
            060400     MOVE  1.58073 TO MIN-RANGE.                                  IF1014.2
            060500     MOVE  1.58085 TO MAX-RANGE.                                  IF1014.2
            060600 F-ACOS-TEST-13.                                                  IF1014.2
            060700     COMPUTE WS-NUM = FUNCTION ACOS( 1 - 1.01).                   IF1014.2
            060800     IF (WS-NUM >= MIN-RANGE) AND                                 IF1014.2
            060900        (WS-NUM <= MAX-RANGE) THEN                                IF1014.2
            061000                    PERFORM PASS                                  IF1014.2
            061100     ELSE                                                         IF1014.2
            061200                    MOVE WS-NUM TO COMPUTED-N                     IF1014.2
            061300                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1014.2
            061400                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1014.2
            061500                    PERFORM FAIL.                                 IF1014.2
            061600     GO TO F-ACOS-WRITE-13.                                       IF1014.2
            061700 F-ACOS-DELETE-13.                                                IF1014.2
            061800     PERFORM  DE-LETE.                                            IF1014.2
            061900     GO TO    F-ACOS-WRITE-13.                                    IF1014.2
            062000 F-ACOS-WRITE-13.                                                 IF1014.2
            062100     MOVE "F-ACOS-13" TO PAR-NAME.                                IF1014.2
            062200     PERFORM  PRINT-DETAIL.                                       IF1014.2
            062300*****************TEST (d) - COMPLEX TEST****************          IF1014.2
            062400 F-ACOS-14.                                                       IF1014.2
            062500     MOVE ZERO TO WS-NUM.                                         IF1014.2
            062600     MOVE  0.141533 TO MIN-RANGE.                                 IF1014.2
            062700     MOVE  0.141545 TO MAX-RANGE.                                 IF1014.2
            062800 F-ACOS-TEST-14.                                                  IF1014.2
            062900     COMPUTE WS-NUM = FUNCTION ACOS(1.98 / 2).                    IF1014.2
            063000     IF (WS-NUM >= MIN-RANGE) AND                                 IF1014.2
            063100        (WS-NUM <= MAX-RANGE) THEN                                IF1014.2
            063200                    PERFORM PASS                                  IF1014.2
            063300     ELSE                                                         IF1014.2
            063400                    MOVE WS-NUM TO COMPUTED-N                     IF1014.2
            063500                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1014.2
            063600                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1014.2
            063700                    PERFORM FAIL.                                 IF1014.2
            063800     GO TO F-ACOS-WRITE-14.                                       IF1014.2
            063900 F-ACOS-DELETE-14.                                                IF1014.2
            064000     PERFORM  DE-LETE.                                            IF1014.2
            064100     GO TO    F-ACOS-WRITE-14.                                    IF1014.2
            064200 F-ACOS-WRITE-14.                                                 IF1014.2
            064300     MOVE "F-ACOS-14" TO PAR-NAME.                                IF1014.2
            064400     PERFORM  PRINT-DETAIL.                                       IF1014.2
            064500*****************TEST (e) - COMPLEX TEST****************          IF1014.2
            064600 F-ACOS-15.                                                       IF1014.2
            064700     MOVE ZERO TO WS-NUM.                                         IF1014.2
            064800     MOVE  1.05866 TO MIN-RANGE.                                  IF1014.2
            064900     MOVE  1.05874 TO MAX-RANGE.                                  IF1014.2
            065000 F-ACOS-TEST-15.                                                  IF1014.2
            065100     COMPUTE WS-NUM = FUNCTION ACOS(0.2 + 0.29).                  IF1014.2
            065200     IF (WS-NUM >= MIN-RANGE) AND                                 IF1014.2
            065300        (WS-NUM <= MAX-RANGE) THEN                                IF1014.2
            065400                    PERFORM PASS                                  IF1014.2
            065500     ELSE                                                         IF1014.2
            065600                    MOVE WS-NUM TO COMPUTED-N                     IF1014.2
            065700                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1014.2
            065800                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1014.2
            065900                    PERFORM FAIL.                                 IF1014.2
            066000     GO TO F-ACOS-WRITE-15.                                       IF1014.2
            066100 F-ACOS-DELETE-15.                                                IF1014.2
            066200     PERFORM  DE-LETE.                                            IF1014.2
            066300     GO TO    F-ACOS-WRITE-15.                                    IF1014.2
            066400 F-ACOS-WRITE-15.                                                 IF1014.2
            066500     MOVE "F-ACOS-15" TO PAR-NAME.                                IF1014.2
            066600     PERFORM  PRINT-DETAIL.                                       IF1014.2
            066700*****************TEST (f) - COMPLEX TEST****************          IF1014.2
            066800 F-ACOS-16.                                                       IF1014.2
            066900     MOVE ZERO TO WS-NUM.                                         IF1014.2
            067000     MOVE  2.99993 TO MIN-RANGE.                                  IF1014.2
            067100     MOVE  3.00017 TO MAX-RANGE.                                  IF1014.2
            067200 F-ACOS-TEST-16.                                                  IF1014.2
            067300     COMPUTE WS-NUM = FUNCTION ACOS(0.99 * -1).                   IF1014.2
            067400     IF (WS-NUM >= MIN-RANGE) AND                                 IF1014.2
            067500        (WS-NUM <= MAX-RANGE) THEN                                IF1014.2
            067600                    PERFORM PASS                                  IF1014.2
            067700     ELSE                                                         IF1014.2
            067800                    MOVE WS-NUM TO COMPUTED-N                     IF1014.2
            067900                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1014.2
            068000                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1014.2
            068100                    PERFORM FAIL.                                 IF1014.2
            068200     GO TO F-ACOS-WRITE-16.                                       IF1014.2
            068300 F-ACOS-DELETE-16.                                                IF1014.2
            068400     PERFORM  DE-LETE.                                            IF1014.2
            068500     GO TO    F-ACOS-WRITE-16.                                    IF1014.2
            068600 F-ACOS-WRITE-16.                                                 IF1014.2
            068700     MOVE "F-ACOS-16" TO PAR-NAME.                                IF1014.2
            068800     PERFORM  PRINT-DETAIL.                                       IF1014.2
            068900*****************TEST (g) - COMPLEX TEST****************          IF1014.2
            069000 F-ACOS-17.                                                       IF1014.2
            069100     MOVE ZERO TO WS-NUM.                                         IF1014.2
            069200     MOVE  -0.000040 TO MIN-RANGE.                                IF1014.2
            069300     MOVE  0.00004 TO MAX-RANGE.                                  IF1014.2
            069400 F-ACOS-TEST-17.                                                  IF1014.2
            069500     COMPUTE WS-NUM = FUNCTION ACOS(IND (B) - 2).                 IF1014.2
            069600     IF (WS-NUM >= MIN-RANGE) AND                                 IF1014.2
            069700        (WS-NUM <= MAX-RANGE) THEN                                IF1014.2
            069800                    PERFORM PASS                                  IF1014.2
            069900     ELSE                                                         IF1014.2
            070000                    MOVE WS-NUM TO COMPUTED-N                     IF1014.2
            070100                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1014.2
            070200                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1014.2
            070300                    PERFORM FAIL.                                 IF1014.2
            070400     GO TO F-ACOS-WRITE-17.                                       IF1014.2
            070500 F-ACOS-DELETE-17.                                                IF1014.2
            070600     PERFORM  DE-LETE.                                            IF1014.2
            070700     GO TO    F-ACOS-WRITE-17.                                    IF1014.2
            070800 F-ACOS-WRITE-17.                                                 IF1014.2
            070900     MOVE "F-ACOS-17" TO PAR-NAME.                                IF1014.2
            071000     PERFORM  PRINT-DETAIL.                                       IF1014.2
            071100*****************TEST (h) - COMPLEX TEST****************          IF1014.2
            071200 F-ACOS-18.                                                       IF1014.2
            071300     MOVE ZERO TO WS-NUM.                                         IF1014.2
            071400     MOVE  0.679646 TO MIN-RANGE.                                 IF1014.2
            071500     MOVE  0.679700 TO MAX-RANGE.                                 IF1014.2
            071600 F-ACOS-TEST-18.                                                  IF1014.2
            071700     COMPUTE WS-NUM = FUNCTION ACOS(IND(5) / 9).                  IF1014.2
            071800     IF (WS-NUM >= MIN-RANGE) AND                                 IF1014.2
            071900        (WS-NUM <= MAX-RANGE) THEN                                IF1014.2
            072000                    PERFORM PASS                                  IF1014.2
            072100     ELSE                                                         IF1014.2
            072200                    MOVE WS-NUM TO COMPUTED-N                     IF1014.2
            072300                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1014.2
            072400                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1014.2
            072500                    PERFORM FAIL.                                 IF1014.2
            072600     GO TO F-ACOS-WRITE-18.                                       IF1014.2
            072700 F-ACOS-DELETE-18.                                                IF1014.2
            072800     PERFORM  DE-LETE.                                            IF1014.2
            072900     GO TO    F-ACOS-WRITE-18.                                    IF1014.2
            073000 F-ACOS-WRITE-18.                                                 IF1014.2
            073100     MOVE "F-ACOS-18" TO PAR-NAME.                                IF1014.2
            073200     PERFORM  PRINT-DETAIL.                                       IF1014.2
            073300*****************TEST (i) - COMPLEX TEST****************          IF1014.2
            073400 F-ACOS-19.                                                       IF1014.2
            073500     MOVE ZERO TO WS-NUM.                                         IF1014.2
            073600     MOVE  0.000000 TO MIN-RANGE.                                 IF1014.2
            073700     MOVE  0.000040 TO MAX-RANGE.                                 IF1014.2
            073800 F-ACOS-TEST-19.                                                  IF1014.2
            073900     COMPUTE WS-NUM = FUNCTION ACOS(4 - 3).                       IF1014.2
            074000     IF (WS-NUM >= MIN-RANGE) AND                                 IF1014.2
            074100        (WS-NUM <= MAX-RANGE) THEN                                IF1014.2
            074200                    PERFORM PASS                                  IF1014.2
            074300     ELSE                                                         IF1014.2
            074400                    MOVE WS-NUM TO COMPUTED-N                     IF1014.2
            074500                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1014.2
            074600                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1014.2
            074700                    PERFORM FAIL.                                 IF1014.2
            074800     GO TO F-ACOS-WRITE-19.                                       IF1014.2
            074900 F-ACOS-DELETE-19.                                                IF1014.2
            075000     PERFORM  DE-LETE.                                            IF1014.2
            075100     GO TO    F-ACOS-WRITE-19.                                    IF1014.2
            075200 F-ACOS-WRITE-19.                                                 IF1014.2
            075300     MOVE "F-ACOS-19" TO PAR-NAME.                                IF1014.2
            075400     PERFORM  PRINT-DETAIL.                                       IF1014.2
            075500*****************TEST (j) - COMPLEX TEST****************          IF1014.2
            075600 F-ACOS-20.                                                       IF1014.2
            075700     MOVE ZERO TO WS-NUM.                                         IF1014.2
            075800     MOVE  0.000000 TO MIN-RANGE.                                 IF1014.2
            075900     MOVE  0.000004 TO MAX-RANGE.                                 IF1014.2
            076000 F-ACOS-TEST-20.                                                  IF1014.2
            076100     COMPUTE WS-NUM = FUNCTION ACOS(C / C).                       IF1014.2
            076200     IF (WS-NUM >= MIN-RANGE) AND                                 IF1014.2
            076300        (WS-NUM <= MAX-RANGE) THEN                                IF1014.2
            076400                    PERFORM PASS                                  IF1014.2
            076500     ELSE                                                         IF1014.2
            076600                    MOVE WS-NUM TO COMPUTED-N                     IF1014.2
            076700                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1014.2
            076800                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1014.2
            076900                    PERFORM FAIL.                                 IF1014.2
            077000     GO TO F-ACOS-WRITE-20.                                       IF1014.2
            077100 F-ACOS-DELETE-20.                                                IF1014.2
            077200     PERFORM  DE-LETE.                                            IF1014.2
            077300     GO TO    F-ACOS-WRITE-20.                                    IF1014.2
            077400 F-ACOS-WRITE-20.                                                 IF1014.2
            077500     MOVE "F-ACOS-20" TO PAR-NAME.                                IF1014.2
            077600     PERFORM  PRINT-DETAIL.                                       IF1014.2
            077700*****************TEST (k) - COMPLEX TEST****************          IF1014.2
            077800 F-ACOS-21.                                                       IF1014.2
            077900     MOVE ZERO TO WS-NUM.                                         IF1014.2
            078000     MOVE  1.31806 TO MIN-RANGE.                                  IF1014.2
            078100     MOVE  1.31816 TO MAX-RANGE.                                  IF1014.2
            078200 F-ACOS-TEST-21.                                                  IF1014.2
            078300     COMPUTE WS-NUM = FUNCTION ACOS(0.25 * 1).                    IF1014.2
            078400     IF (WS-NUM >= MIN-RANGE) AND                                 IF1014.2
            078500        (WS-NUM <= MAX-RANGE) THEN                                IF1014.2
            078600                    PERFORM PASS                                  IF1014.2
            078700     ELSE                                                         IF1014.2
            078800                    MOVE WS-NUM TO COMPUTED-N                     IF1014.2
            078900                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1014.2
            079000                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1014.2
            079100                    PERFORM FAIL.                                 IF1014.2
            079200     GO TO F-ACOS-WRITE-21.                                       IF1014.2
            079300 F-ACOS-DELETE-21.                                                IF1014.2
            079400     PERFORM  DE-LETE.                                            IF1014.2
            079500     GO TO    F-ACOS-WRITE-21.                                    IF1014.2
            079600 F-ACOS-WRITE-21.                                                 IF1014.2
            079700     MOVE "F-ACOS-21" TO PAR-NAME.                                IF1014.2
            079800     PERFORM  PRINT-DETAIL.                                       IF1014.2
            079900*****************TEST (l) - COMPLEX TEST****************          IF1014.2
            080000 F-ACOS-22.                                                       IF1014.2
            080100     MOVE ZERO TO WS-NUM.                                         IF1014.2
            080200     MOVE  1.57073 TO MIN-RANGE.                                  IF1014.2
            080300     MOVE  1.57085 TO MAX-RANGE.                                  IF1014.2
            080400 F-ACOS-TEST-22.                                                  IF1014.2
            080500     COMPUTE WS-NUM = FUNCTION ACOS((D / D) - 1).                 IF1014.2
            080600     IF (WS-NUM >= MIN-RANGE) AND                                 IF1014.2
            080700        (WS-NUM <= MAX-RANGE) THEN                                IF1014.2
            080800                    PERFORM PASS                                  IF1014.2
            080900     ELSE                                                         IF1014.2
            081000                    MOVE WS-NUM TO COMPUTED-N                     IF1014.2
            081100                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1014.2
            081200                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1014.2
            081300                    PERFORM FAIL.                                 IF1014.2
            081400     GO TO F-ACOS-WRITE-22.                                       IF1014.2
            081500 F-ACOS-DELETE-22.                                                IF1014.2
            081600     PERFORM  DE-LETE.                                            IF1014.2
            081700     GO TO    F-ACOS-WRITE-22.                                    IF1014.2
            081800 F-ACOS-WRITE-22.                                                 IF1014.2
            081900     MOVE "F-ACOS-22" TO PAR-NAME.                                IF1014.2
            082000     PERFORM  PRINT-DETAIL.                                       IF1014.2
            082100*****************TEST (m) - COMPLEX TEST****************          IF1014.2
            082200 F-ACOS-23.                                                       IF1014.2
            082300     MOVE ZERO TO WS-NUM.                                         IF1014.2
            082400     MOVE  2.60285  TO MIN-RANGE.                                 IF1014.2
            082500     MOVE  2.60305 TO MAX-RANGE.                                  IF1014.2
            082600 F-ACOS-TEST-23.                                                  IF1014.2
            082700     COMPUTE WS-NUM = FUNCTION ACOS(PI - 4).                      IF1014.2
            082800     IF (WS-NUM >= MIN-RANGE) AND                                 IF1014.2
            082900        (WS-NUM <= MAX-RANGE) THEN                                IF1014.2
            083000                    PERFORM PASS                                  IF1014.2
            083100     ELSE                                                         IF1014.2
            083200                    MOVE WS-NUM TO COMPUTED-N                     IF1014.2
            083300                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1014.2
            083400                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1014.2
            083500                    PERFORM FAIL.                                 IF1014.2
            083600     GO TO F-ACOS-WRITE-23.                                       IF1014.2
            083700 F-ACOS-DELETE-23.                                                IF1014.2
            083800     PERFORM  DE-LETE.                                            IF1014.2
            083900     GO TO    F-ACOS-WRITE-23.                                    IF1014.2
            084000 F-ACOS-WRITE-23.                                                 IF1014.2
            084100     MOVE "F-ACOS-23" TO PAR-NAME.                                IF1014.2
            084200     PERFORM  PRINT-DETAIL.                                       IF1014.2
            084300*****************TEST (n) - COMPLEX TEST****************          IF1014.2
            084400 F-ACOS-24.                                                       IF1014.2
            084500     MOVE ZERO TO WS-NUM.                                         IF1014.2
            084600     MOVE 1.57073 TO MIN-RANGE.                                   IF1014.2
            084700     MOVE 1.57085 TO MAX-RANGE.                                   IF1014.2
            084800 F-ACOS-TEST-24.                                                  IF1014.2
            084900     COMPUTE WS-NUM = FUNCTION ACOS(FUNCTION ACOS(D / D)).        IF1014.2
            085000     IF (WS-NUM >= MIN-RANGE) AND                                 IF1014.2
            085100        (WS-NUM <= MAX-RANGE) THEN                                IF1014.2
            085200                    PERFORM PASS                                  IF1014.2
            085300     ELSE                                                         IF1014.2
            085400                    MOVE WS-NUM TO COMPUTED-N                     IF1014.2
            085500                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1014.2
            085600                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1014.2
            085700                    PERFORM FAIL.                                 IF1014.2
            085800     GO TO F-ACOS-WRITE-24.                                       IF1014.2
            085900 F-ACOS-DELETE-24.                                                IF1014.2
            086000     PERFORM  DE-LETE.                                            IF1014.2
            086100     GO TO    F-ACOS-WRITE-24.                                    IF1014.2
            086200 F-ACOS-WRITE-24.                                                 IF1014.2
            086300     MOVE "F-ACOS-24" TO PAR-NAME.                                IF1014.2
            086400     PERFORM  PRINT-DETAIL.                                       IF1014.2
            086500*****************TEST (o) - COMPLEX TEST****************          IF1014.2
            086600 F-ACOS-25.                                                       IF1014.2
            086700     MOVE ZERO TO WS-NUM.                                         IF1014.2
            086800     MOVE 0.000000 TO MIN-RANGE.                                  IF1014.2
            086900     MOVE 0.000040 TO MAX-RANGE.                                  IF1014.2
            087000 F-ACOS-TEST-25.                                                  IF1014.2
            087100     COMPUTE WS-NUM = FUNCTION ACOS(D / D) + FUNCTION ACOS(D / D).IF1014.2
            087200                                                                  IF1014.2
            087300     IF (WS-NUM >= MIN-RANGE) AND                                 IF1014.2
            087400        (WS-NUM <= MAX-RANGE) THEN                                IF1014.2
            087500                    PERFORM PASS                                  IF1014.2
            087600     ELSE                                                         IF1014.2
            087700                    MOVE WS-NUM TO COMPUTED-N                     IF1014.2
            087800                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1014.2
            087900                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1014.2
            088000                    PERFORM FAIL.                                 IF1014.2
            088100     GO TO F-ACOS-WRITE-25.                                       IF1014.2
            088200 F-ACOS-DELETE-25.                                                IF1014.2
            088300     PERFORM  DE-LETE.                                            IF1014.2
            088400     GO TO    F-ACOS-WRITE-25.                                    IF1014.2
            088500 F-ACOS-WRITE-25.                                                 IF1014.2
            088600     MOVE "F-ACOS-25" TO PAR-NAME.                                IF1014.2
            088700     PERFORM  PRINT-DETAIL.                                       IF1014.2
            088800*****************SPECIAL PERFORM TEST**********************       IF1014.2
            088900 F-ACOS-26.                                                       IF1014.2
            089000     MOVE ZERO TO ARG1.                                           IF1014.2
            089100     PERFORM F-ACOS-TEST-26                                       IF1014.2
            089200       UNTIL FUNCTION ACOS(ARG1) < 1.                             IF1014.2
            089300     PERFORM PASS.                                                IF1014.2
            089400     GO TO F-ACOS-WRITE-26.                                       IF1014.2
            089500 F-ACOS-TEST-26.                                                  IF1014.2
            089600     COMPUTE ARG1 = ARG1 + 0.25.                                  IF1014.2
            089700 F-ACOS-DELETE-26.                                                IF1014.2
            089800     PERFORM  DE-LETE.                                            IF1014.2
            089900     GO TO    F-ACOS-WRITE-26.                                    IF1014.2
            090000 F-ACOS-WRITE-26.                                                 IF1014.2
            090100     MOVE "F-ACOS-26" TO PAR-NAME.                                IF1014.2
            090200     PERFORM  PRINT-DETAIL.                                       IF1014.2
            090300********************END OF TESTS***************                   IF1014.2
            090400 CCVS-EXIT SECTION.                                               IF1014.2
            090500 CCVS-999999.                                                     IF1014.2
            090600     GO TO CLOSE-FILES.                                           IF1014.2
        """)
    )

    @Test
    fun if1024_2() = rewriteRun(
        cobol("""
                  *HEADER,COBOL,IF102A                                                            
            000100 IDENTIFICATION DIVISION.                                         IF1024.2
            000200 PROGRAM-ID.                                                      IF1024.2
            000300     IF102A.                                                      IF1024.2
            000400                                                                  IF1024.2
            000500***********************************************************       IF1024.2
            000600*                                                         *       IF1024.2
            000700*   This program is intended to form part of the CCVS85   *       IF1024.2
            000800*   COBOL Test Suite. It contains tests for the           *       IF1024.2
            000900*   Intrinsic Function ANNUITY.                           *       IF1024.2
            001000*                                                         *       IF1024.2
            001100***********************************************************       IF1024.2
            001200 ENVIRONMENT DIVISION.                                            IF1024.2
            001300 CONFIGURATION SECTION.                                           IF1024.2
            001400 SOURCE-COMPUTER.                                                 IF1024.2
            001500     XXXXX082.                                                    IF1024.2
            001600 OBJECT-COMPUTER.                                                 IF1024.2
            001700     XXXXX083.                                                    IF1024.2
            001800 INPUT-OUTPUT SECTION.                                            IF1024.2
            001900 FILE-CONTROL.                                                    IF1024.2
            002000     SELECT PRINT-FILE ASSIGN TO                                  IF1024.2
            002100     XXXXX055.                                                    IF1024.2
            002200 DATA DIVISION.                                                   IF1024.2
            002300 FILE SECTION.                                                    IF1024.2
            002400 FD  PRINT-FILE.                                                  IF1024.2
            002500 01  PRINT-REC PICTURE X(120).                                    IF1024.2
            002600 01  DUMMY-RECORD PICTURE X(120).                                 IF1024.2
            002700 WORKING-STORAGE SECTION.                                         IF1024.2
            002800***********************************************************       IF1024.2
            002900* Variables specific to the Intrinsic Function Test IF102A*       IF1024.2
            003000***********************************************************       IF1024.2
            003100 01  A                   PIC S9(10)          VALUE 4.             IF1024.2
            003200 01  B                   PIC S9(5)V9(5)      VALUE .25.           IF1024.2
            003300 01  C                   PIC S9(10)          VALUE 10.            IF1024.2
            003400 01  D                   PIC S9(10)          VALUE 100.           IF1024.2
            003500 01  ARG2                PIC S9(10)          VALUE 1.             IF1024.2
            003600 01  ARR                                     VALUE "40537".       IF1024.2
            003700     02  IND OCCURS 5 TIMES PIC 9.                                IF1024.2
            003800 01  TEMP                PIC S9(5)V9(5).                          IF1024.2
            003900 01  WS-NUM              PIC S9(5)V9(6).                          IF1024.2
            004000 01  MIN-RANGE           PIC S9(5)V9(7).                          IF1024.2
            004100 01  MAX-RANGE           PIC S9(5)V9(7).                          IF1024.2
            004200*                                                                 IF1024.2
            004300**********************************************************        IF1024.2
            004400*                                                                 IF1024.2
            004500 01  TEST-RESULTS.                                                IF1024.2
            004600     02 FILLER                   PIC X      VALUE SPACE.          IF1024.2
            004700     02 FEATURE                  PIC X(20)  VALUE SPACE.          IF1024.2
            004800     02 FILLER                   PIC X      VALUE SPACE.          IF1024.2
            004900     02 P-OR-F                   PIC X(5)   VALUE SPACE.          IF1024.2
            005000     02 FILLER                   PIC X      VALUE SPACE.          IF1024.2
            005100     02  PAR-NAME.                                                IF1024.2
            005200       03 FILLER                 PIC X(19)  VALUE SPACE.          IF1024.2
            005300       03  PARDOT-X              PIC X      VALUE SPACE.          IF1024.2
            005400       03 DOTVALUE               PIC 99     VALUE ZERO.           IF1024.2
            005500     02 FILLER                   PIC X(8)   VALUE SPACE.          IF1024.2
            005600     02 RE-MARK                  PIC X(61).                       IF1024.2
            005700 01  TEST-COMPUTED.                                               IF1024.2
            005800     02 FILLER                   PIC X(30)  VALUE SPACE.          IF1024.2
            005900     02 FILLER                   PIC X(17)  VALUE                 IF1024.2
            006000            "       COMPUTED=".                                   IF1024.2
            006100     02 COMPUTED-X.                                               IF1024.2
            006200     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          IF1024.2
            006300     03 COMPUTED-N               REDEFINES COMPUTED-A             IF1024.2
            006400                                 PIC -9(9).9(9).                  IF1024.2
            006500     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         IF1024.2
            006600     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     IF1024.2
            006700     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     IF1024.2
            006800     03       CM-18V0 REDEFINES COMPUTED-A.                       IF1024.2
            006900         04 COMPUTED-18V0                    PIC -9(18).          IF1024.2
            007000         04 FILLER                           PIC X.               IF1024.2
            007100     03 FILLER PIC X(50) VALUE SPACE.                             IF1024.2
            007200 01  TEST-CORRECT.                                                IF1024.2
            007300     02 FILLER PIC X(30) VALUE SPACE.                             IF1024.2
            007400     02 FILLER PIC X(17) VALUE "       CORRECT =".                IF1024.2
            007500     02 CORRECT-X.                                                IF1024.2
            007600     03 CORRECT-A                  PIC X(20) VALUE SPACE.         IF1024.2
            007700     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      IF1024.2
            007800     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         IF1024.2
            007900     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     IF1024.2
            008000     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     IF1024.2
            008100     03      CR-18V0 REDEFINES CORRECT-A.                         IF1024.2
            008200         04 CORRECT-18V0                     PIC -9(18).          IF1024.2
            008300         04 FILLER                           PIC X.               IF1024.2
            008400     03 FILLER PIC X(2) VALUE SPACE.                              IF1024.2
            008500     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     IF1024.2
            008600 01  TEST-CORRECT-MIN.                                            IF1024.2
            008700     02 FILLER PIC X(30) VALUE SPACE.                             IF1024.2
            008800     02 FILLER PIC X(17) VALUE "     MIN VALUE =".                IF1024.2
            008900     02 CORRECTMI-X.                                              IF1024.2
            009000     03 CORRECTMI-A                 PIC X(20) VALUE SPACE.        IF1024.2
            009100     03 CORRECT-MIN    REDEFINES CORRECTMI-A     PIC -9(9).9(9).  IF1024.2
            009200     03 CORRECTMI-0V18 REDEFINES CORRECTMI-A     PIC -.9(18).     IF1024.2
            009300     03 CORRECTMI-4V14 REDEFINES CORRECTMI-A     PIC -9(4).9(14). IF1024.2
            009400     03 CORRECTMI-14V4 REDEFINES CORRECTMI-A     PIC -9(14).9(4). IF1024.2
            009500     03      CR-18V0 REDEFINES CORRECTMI-A.                       IF1024.2
            009600         04 CORRECTMI-18V0                     PIC -9(18).        IF1024.2
            009700         04 FILLER                           PIC X.               IF1024.2
            009800     03 FILLER PIC X(2) VALUE SPACE.                              IF1024.2
            009900     03 FILLER                           PIC X(48) VALUE SPACE.   IF1024.2
            010000 01  TEST-CORRECT-MAX.                                            IF1024.2
            010100     02 FILLER PIC X(30) VALUE SPACE.                             IF1024.2
            010200     02 FILLER PIC X(17) VALUE "     MAX VALUE =".                IF1024.2
            010300     02 CORRECTMA-X.                                              IF1024.2
            010400     03 CORRECTMA-A                  PIC X(20) VALUE SPACE.       IF1024.2
            010500     03 CORRECT-MAX    REDEFINES CORRECTMA-A     PIC -9(9).9(9).  IF1024.2
            010600     03 CORRECTMA-0V18 REDEFINES CORRECTMA-A     PIC -.9(18).     IF1024.2
            010700     03 CORRECTMA-4V14 REDEFINES CORRECTMA-A     PIC -9(4).9(14). IF1024.2
            010800     03 CORRECTMA-14V4 REDEFINES CORRECTMA-A     PIC -9(14).9(4). IF1024.2
            010900     03      CR-18V0 REDEFINES CORRECTMA-A.                       IF1024.2
            011000         04 CORRECTMA-18V0                     PIC -9(18).        IF1024.2
            011100         04 FILLER                           PIC X.               IF1024.2
            011200     03 FILLER PIC X(2) VALUE SPACE.                              IF1024.2
            011300     03 CORMA-ANSI-REFERENCE             PIC X(48) VALUE SPACE.   IF1024.2
            011400 01  CCVS-C-1.                                                    IF1024.2
            011500     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PAIF1024.2
            011600-    "SS  PARAGRAPH-NAME                                          IF1024.2
            011700-    "       REMARKS".                                            IF1024.2
            011800     02 FILLER                     PIC X(20)    VALUE SPACE.      IF1024.2
            011900 01  CCVS-C-2.                                                    IF1024.2
            012000     02 FILLER                     PIC X        VALUE SPACE.      IF1024.2
            012100     02 FILLER                     PIC X(6)     VALUE "TESTED".   IF1024.2
            012200     02 FILLER                     PIC X(15)    VALUE SPACE.      IF1024.2
            012300     02 FILLER                     PIC X(4)     VALUE "FAIL".     IF1024.2
            012400     02 FILLER                     PIC X(94)    VALUE SPACE.      IF1024.2
            012500 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       IF1024.2
            012600 01  REC-CT                        PIC 99       VALUE ZERO.       IF1024.2
            012700 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       IF1024.2
            012800 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       IF1024.2
            012900 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       IF1024.2
            013000 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       IF1024.2
            013100 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       IF1024.2
            013200 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       IF1024.2
            013300 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      IF1024.2
            013400 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       IF1024.2
            013500 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     IF1024.2
            013600 01  CCVS-H-1.                                                    IF1024.2
            013700     02  FILLER                    PIC X(39)    VALUE SPACES.     IF1024.2
            013800     02  FILLER                    PIC X(42)    VALUE             IF1024.2
            013900     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 IF1024.2
            014000     02  FILLER                    PIC X(39)    VALUE SPACES.     IF1024.2
            014100 01  CCVS-H-2A.                                                   IF1024.2
            014200   02  FILLER                        PIC X(40)  VALUE SPACE.      IF1024.2
            014300   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  IF1024.2
            014400   02  FILLER                        PIC XXXX   VALUE             IF1024.2
            014500     "4.2 ".                                                      IF1024.2
            014600   02  FILLER                        PIC X(28)  VALUE             IF1024.2
            014700            " COPY - NOT FOR DISTRIBUTION".                       IF1024.2
            014800   02  FILLER                        PIC X(41)  VALUE SPACE.      IF1024.2
            014900                                                                  IF1024.2
            015000 01  CCVS-H-2B.                                                   IF1024.2
            015100   02  FILLER                        PIC X(15)  VALUE             IF1024.2
            015200            "TEST RESULT OF ".                                    IF1024.2
            015300   02  TEST-ID                       PIC X(9).                    IF1024.2
            015400   02  FILLER                        PIC X(4)   VALUE             IF1024.2
            015500            " IN ".                                               IF1024.2
            015600   02  FILLER                        PIC X(12)  VALUE             IF1024.2
            015700     " HIGH       ".                                              IF1024.2
            015800   02  FILLER                        PIC X(22)  VALUE             IF1024.2
            015900            " LEVEL VALIDATION FOR ".                             IF1024.2
            016000   02  FILLER                        PIC X(58)  VALUE             IF1024.2
            016100     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".IF1024.2
            016200 01  CCVS-H-3.                                                    IF1024.2
            016300     02  FILLER                      PIC X(34)  VALUE             IF1024.2
            016400            " FOR OFFICIAL USE ONLY    ".                         IF1024.2
            016500     02  FILLER                      PIC X(58)  VALUE             IF1024.2
            016600     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".IF1024.2
            016700     02  FILLER                      PIC X(28)  VALUE             IF1024.2
            016800            "  COPYRIGHT   1985 ".                                IF1024.2
            016900 01  CCVS-E-1.                                                    IF1024.2
            017000     02 FILLER                       PIC X(52)  VALUE SPACE.      IF1024.2
            017100     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              IF1024.2
            017200     02 ID-AGAIN                     PIC X(9).                    IF1024.2
            017300     02 FILLER                       PIC X(45)  VALUE SPACES.     IF1024.2
            017400 01  CCVS-E-2.                                                    IF1024.2
            017500     02  FILLER                      PIC X(31)  VALUE SPACE.      IF1024.2
            017600     02  FILLER                      PIC X(21)  VALUE SPACE.      IF1024.2
            017700     02 CCVS-E-2-2.                                               IF1024.2
            017800         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      IF1024.2
            017900         03 FILLER                   PIC X      VALUE SPACE.      IF1024.2
            018000         03 ENDER-DESC               PIC X(44)  VALUE             IF1024.2
            018100            "ERRORS ENCOUNTERED".                                 IF1024.2
            018200 01  CCVS-E-3.                                                    IF1024.2
            018300     02  FILLER                      PIC X(22)  VALUE             IF1024.2
            018400            " FOR OFFICIAL USE ONLY".                             IF1024.2
            018500     02  FILLER                      PIC X(12)  VALUE SPACE.      IF1024.2
            018600     02  FILLER                      PIC X(58)  VALUE             IF1024.2
            018700     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".IF1024.2
            018800     02  FILLER                      PIC X(13)  VALUE SPACE.      IF1024.2
            018900     02 FILLER                       PIC X(15)  VALUE             IF1024.2
            019000             " COPYRIGHT 1985".                                   IF1024.2
            019100 01  CCVS-E-4.                                                    IF1024.2
            019200     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      IF1024.2
            019300     02 FILLER                       PIC X(4)   VALUE " OF ".     IF1024.2
            019400     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      IF1024.2
            019500     02 FILLER                       PIC X(40)  VALUE             IF1024.2
            019600      "  TESTS WERE EXECUTED SUCCESSFULLY".                       IF1024.2
            019700 01  XXINFO.                                                      IF1024.2
            019800     02 FILLER                       PIC X(19)  VALUE             IF1024.2
            019900            "*** INFORMATION ***".                                IF1024.2
            020000     02 INFO-TEXT.                                                IF1024.2
            020100       04 FILLER                     PIC X(8)   VALUE SPACE.      IF1024.2
            020200       04 XXCOMPUTED                 PIC X(20).                   IF1024.2
            020300       04 FILLER                     PIC X(5)   VALUE SPACE.      IF1024.2
            020400       04 XXCORRECT                  PIC X(20).                   IF1024.2
            020500     02 INF-ANSI-REFERENCE           PIC X(48).                   IF1024.2
            020600 01  HYPHEN-LINE.                                                 IF1024.2
            020700     02 FILLER  PIC IS X VALUE IS SPACE.                          IF1024.2
            020800     02 FILLER  PIC IS X(65)    VALUE IS "************************IF1024.2
            020900-    "*****************************************".                 IF1024.2
            021000     02 FILLER  PIC IS X(54)    VALUE IS "************************IF1024.2
            021100-    "******************************".                            IF1024.2
            021200 01  CCVS-PGM-ID                     PIC X(9)   VALUE             IF1024.2
            021300     "IF102A".                                                    IF1024.2
            021400 PROCEDURE DIVISION.                                              IF1024.2
            021500 CCVS1 SECTION.                                                   IF1024.2
            021600 OPEN-FILES.                                                      IF1024.2
            021700     OPEN     OUTPUT PRINT-FILE.                                  IF1024.2
            021800     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   IF1024.2
            021900     MOVE    SPACE TO TEST-RESULTS.                               IF1024.2
            022000     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             IF1024.2
            022100     GO TO CCVS1-EXIT.                                            IF1024.2
            022200 CLOSE-FILES.                                                     IF1024.2
            022300     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   IF1024.2
            022400 TERMINATE-CCVS.                                                  IF1024.2
            022500     STOP     RUN.                                                IF1024.2
            022600 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         IF1024.2
            022700 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           IF1024.2
            022800 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          IF1024.2
            022900 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      IF1024.2
            023000     MOVE "****TEST DELETED****" TO RE-MARK.                      IF1024.2
            023100 PRINT-DETAIL.                                                    IF1024.2
            023200     IF REC-CT NOT EQUAL TO ZERO                                  IF1024.2
            023300             MOVE "." TO PARDOT-X                                 IF1024.2
            023400             MOVE REC-CT TO DOTVALUE.                             IF1024.2
            023500     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      IF1024.2
            023600     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               IF1024.2
            023700        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 IF1024.2
            023800          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 IF1024.2
            023900     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              IF1024.2
            024000     MOVE SPACE TO CORRECT-X.                                     IF1024.2
            024100     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         IF1024.2
            024200     MOVE     SPACE TO RE-MARK.                                   IF1024.2
            024300 HEAD-ROUTINE.                                                    IF1024.2
            024400     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  IF1024.2
            024500     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  IF1024.2
            024600     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  IF1024.2
            024700     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  IF1024.2
            024800 COLUMN-NAMES-ROUTINE.                                            IF1024.2
            024900     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           IF1024.2
            025000     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1024.2
            025100     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        IF1024.2
            025200 END-ROUTINE.                                                     IF1024.2
            025300     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.IF1024.2
            025400 END-RTN-EXIT.                                                    IF1024.2
            025500     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1024.2
            025600 END-ROUTINE-1.                                                   IF1024.2
            025700      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      IF1024.2
            025800      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               IF1024.2
            025900      ADD PASS-COUNTER TO ERROR-HOLD.                             IF1024.2
            026000      MOVE PASS-COUNTER TO CCVS-E-4-1.                            IF1024.2
            026100      MOVE ERROR-HOLD TO CCVS-E-4-2.                              IF1024.2
            026200      MOVE CCVS-E-4 TO CCVS-E-2-2.                                IF1024.2
            026300      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           IF1024.2
            026400  END-ROUTINE-12.                                                 IF1024.2
            026500      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        IF1024.2
            026600     IF       ERROR-COUNTER IS EQUAL TO ZERO                      IF1024.2
            026700         MOVE "NO " TO ERROR-TOTAL                                IF1024.2
            026800         ELSE                                                     IF1024.2
            026900         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       IF1024.2
            027000     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           IF1024.2
            027100     PERFORM WRITE-LINE.                                          IF1024.2
            027200 END-ROUTINE-13.                                                  IF1024.2
            027300     IF DELETE-COUNTER IS EQUAL TO ZERO                           IF1024.2
            027400         MOVE "NO " TO ERROR-TOTAL  ELSE                          IF1024.2
            027500         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      IF1024.2
            027600     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   IF1024.2
            027700     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           IF1024.2
            027800      IF   INSPECT-COUNTER EQUAL TO ZERO                          IF1024.2
            027900          MOVE "NO " TO ERROR-TOTAL                               IF1024.2
            028000      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   IF1024.2
            028100      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            IF1024.2
            028200      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          IF1024.2
            028300     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           IF1024.2
            028400 WRITE-LINE.                                                      IF1024.2
            028500     ADD 1 TO RECORD-COUNT.                                       IF1024.2
            028600*    IF RECORD-COUNT GREATER 42                                   IF1024.2
            028700*        MOVE DUMMY-RECORD TO DUMMY-HOLD                          IF1024.2
            028800*        MOVE SPACE TO DUMMY-RECORD                               IF1024.2
            028900*        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  IF1024.2
            029000*        MOVE CCVS-H-1  TO DUMMY-RECORD  PERFORM WRT-LN 2 TIMES   IF1024.2
            029100*        MOVE CCVS-H-2A TO DUMMY-RECORD  PERFORM WRT-LN 2 TIMES   IF1024.2
            029200*        MOVE CCVS-H-2B TO DUMMY-RECORD  PERFORM WRT-LN 3 TIMES   IF1024.2
            029300*        MOVE CCVS-H-3  TO DUMMY-RECORD  PERFORM WRT-LN 3 TIMES   IF1024.2
            029400*        MOVE CCVS-C-1  TO DUMMY-RECORD  PERFORM WRT-LN           IF1024.2
            029500*        MOVE CCVS-C-2  TO DUMMY-RECORD  PERFORM WRT-LN           IF1024.2
            029600*        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          IF1024.2
            029700*        MOVE DUMMY-HOLD TO DUMMY-RECORD                          IF1024.2
            029800*        MOVE ZERO TO RECORD-COUNT.                               IF1024.2
            029900     PERFORM WRT-LN.                                              IF1024.2
            030000 WRT-LN.                                                          IF1024.2
            030100     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               IF1024.2
            030200     MOVE SPACE TO DUMMY-RECORD.                                  IF1024.2
            030300 BLANK-LINE-PRINT.                                                IF1024.2
            030400     PERFORM WRT-LN.                                              IF1024.2
            030500 FAIL-ROUTINE.                                                    IF1024.2
            030600     IF     COMPUTED-X NOT EQUAL TO SPACE                         IF1024.2
            030700            GO TO FAIL-ROUTINE-WRITE.                             IF1024.2
            030800     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.IF1024.2
            030900     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 IF1024.2
            031000     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   IF1024.2
            031100     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1024.2
            031200     MOVE   SPACES TO INF-ANSI-REFERENCE.                         IF1024.2
            031300     GO TO  FAIL-ROUTINE-EX.                                      IF1024.2
            031400 FAIL-ROUTINE-WRITE.                                              IF1024.2
            031500     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE.        IF1024.2
            031600     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE                  IF1024.2
            031700                              CORMA-ANSI-REFERENCE.               IF1024.2
            031800     IF CORRECT-MIN NOT EQUAL TO SPACES THEN                      IF1024.2
            031900           MOVE TEST-CORRECT-MIN TO PRINT-REC PERFORM WRITE-LINE  IF1024.2
            032000           MOVE TEST-CORRECT-MAX TO PRINT-REC PERFORM WRITE-LINE  IF1024.2
            032100     ELSE                                                         IF1024.2
            032200           MOVE TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE.     IF1024.2
            032300     PERFORM WRITE-LINE.                                          IF1024.2
            032400     MOVE   SPACES TO COR-ANSI-REFERENCE.                         IF1024.2
            032500 FAIL-ROUTINE-EX. EXIT.                                           IF1024.2
            032600 BAIL-OUT.                                                        IF1024.2
            032700     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   IF1024.2
            032800     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           IF1024.2
            032900 BAIL-OUT-WRITE.                                                  IF1024.2
            033000     MOVE CORRECT-A TO XXCORRECT.                                 IF1024.2
            033100     MOVE COMPUTED-A TO XXCOMPUTED.                               IF1024.2
            033200     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 IF1024.2
            033300     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1024.2
            033400     MOVE   SPACES TO INF-ANSI-REFERENCE.                         IF1024.2
            033500 BAIL-OUT-EX. EXIT.                                               IF1024.2
            033600 CCVS1-EXIT.                                                      IF1024.2
            033700     EXIT.                                                        IF1024.2
            033800********************************************************          IF1024.2
            033900*                                                      *          IF1024.2
            034000*    Intrinsic Function Tests         IF102A - ANNUITY *          IF1024.2
            034100*                                                      *          IF1024.2
            034200********************************************************          IF1024.2
            034300 SECT-IF102A SECTION.                                             IF1024.2
            034400 F-ANNUITY-INFO.                                                  IF1024.2
            034500     MOVE     "See ref. A-34 2.6" TO ANSI-REFERENCE.              IF1024.2
            034600     MOVE     "ANNUITY Function" TO FEATURE.                      IF1024.2
            034700*****************TEST (a) - SIMPLE TEST*****************          IF1024.2
            034800 F-ANNUITY-01.                                                    IF1024.2
            034900     MOVE ZERO TO WS-NUM.                                         IF1024.2
            035000     MOVE  0.249995 TO MIN-RANGE.                                 IF1024.2
            035100     MOVE  0.250005 TO MAX-RANGE.                                 IF1024.2
            035200 F-ANNUITY-TEST-01.                                               IF1024.2
            035300     COMPUTE WS-NUM = FUNCTION ANNUITY(0, 4).                     IF1024.2
            035400     IF (WS-NUM >= MIN-RANGE) AND                                 IF1024.2
            035500        (WS-NUM <= MAX-RANGE) THEN                                IF1024.2
            035600                    PERFORM PASS                                  IF1024.2
            035700     ELSE                                                         IF1024.2
            035800                    MOVE WS-NUM TO COMPUTED-N                     IF1024.2
            035900                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1024.2
            036000                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1024.2
            036100                    PERFORM FAIL.                                 IF1024.2
            036200     GO TO F-ANNUITY-WRITE-01.                                    IF1024.2
            036300 F-ANNUITY-DELETE-01.                                             IF1024.2
            036400     PERFORM  DE-LETE.                                            IF1024.2
            036500     GO TO    F-ANNUITY-WRITE-01.                                 IF1024.2
            036600 F-ANNUITY-WRITE-01.                                              IF1024.2
            036700     MOVE "F-ANNUITY-01" TO PAR-NAME.                             IF1024.2
            036800     PERFORM  PRINT-DETAIL.                                       IF1024.2
            036900*****************TEST (b) - SIMPLE TEST*****************          IF1024.2
            037000 F-ANNUITY-02.                                                    IF1024.2
            037100     EVALUATE FUNCTION ANNUITY(2.9, 4)                            IF1024.2
            037200     WHEN  2.91252 THRU  2.91264                                  IF1024.2
            037300                    PERFORM PASS                                  IF1024.2
            037400     WHEN OTHER                                                   IF1024.2
            037500                    PERFORM FAIL.                                 IF1024.2
            037600     GO TO F-ANNUITY-WRITE-02.                                    IF1024.2
            037700 F-ANNUITY-DELETE-02.                                             IF1024.2
            037800     PERFORM  DE-LETE.                                            IF1024.2
            037900     GO TO    F-ANNUITY-WRITE-02.                                 IF1024.2
            038000 F-ANNUITY-WRITE-02.                                              IF1024.2
            038100     MOVE "F-ANNUITY-02" TO PAR-NAME.                             IF1024.2
            038200     PERFORM  PRINT-DETAIL.                                       IF1024.2
            038300*****************TEST (c) - SIMPLE TEST*****************          IF1024.2
            038400 F-ANNUITY-03.                                                    IF1024.2
            038500     MOVE  0.308663 TO MIN-RANGE.                                 IF1024.2
            038600     MOVE  0.308675 TO MAX-RANGE.                                 IF1024.2
            038700 F-ANNUITY-TEST-03.                                               IF1024.2
            038800     IF (FUNCTION ANNUITY(.09, A) >= MIN-RANGE) AND               IF1024.2
            038900        (FUNCTION ANNUITY(.09, A) <= MAX-RANGE) THEN              IF1024.2
            039000                    PERFORM PASS                                  IF1024.2
            039100     ELSE                                                         IF1024.2
            039200                    PERFORM FAIL.                                 IF1024.2
            039300     GO TO F-ANNUITY-WRITE-03.                                    IF1024.2
            039400 F-ANNUITY-DELETE-03.                                             IF1024.2
            039500     PERFORM  DE-LETE.                                            IF1024.2
            039600     GO TO    F-ANNUITY-WRITE-03.                                 IF1024.2
            039700 F-ANNUITY-WRITE-03.                                              IF1024.2
            039800     MOVE "F-ANNUITY-03" TO PAR-NAME.                             IF1024.2
            039900     PERFORM  PRINT-DETAIL.                                       IF1024.2
            040000*****************TEST (d) - SIMPLE TEST*****************          IF1024.2
            040100 F-ANNUITY-04.                                                    IF1024.2
            040200     MOVE ZERO TO WS-NUM.                                         IF1024.2
            040300     MOVE  0.694430 TO MIN-RANGE.                                 IF1024.2
            040400     MOVE  0.694458 TO MAX-RANGE.                                 IF1024.2
            040500 F-ANNUITY-TEST-04.                                               IF1024.2
            040600     COMPUTE WS-NUM = FUNCTION ANNUITY(B, 2).                     IF1024.2
            040700     IF (WS-NUM >= MIN-RANGE) AND                                 IF1024.2
            040800        (WS-NUM <= MAX-RANGE) THEN                                IF1024.2
            040900                    PERFORM PASS                                  IF1024.2
            041000     ELSE                                                         IF1024.2
            041100                    MOVE WS-NUM TO COMPUTED-N                     IF1024.2
            041200                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1024.2
            041300                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1024.2
            041400                    PERFORM FAIL.                                 IF1024.2
            041500     GO TO F-ANNUITY-WRITE-04.                                    IF1024.2
            041600 F-ANNUITY-DELETE-04.                                             IF1024.2
            041700     PERFORM  DE-LETE.                                            IF1024.2
            041800     GO TO    F-ANNUITY-WRITE-04.                                 IF1024.2
            041900 F-ANNUITY-WRITE-04.                                              IF1024.2
            042000     MOVE "F-ANNUITY-04" TO PAR-NAME.                             IF1024.2
            042100     PERFORM  PRINT-DETAIL.                                       IF1024.2
            042200*****************TEST (e) - SIMPLE TEST*****************          IF1024.2
            042300 F-ANNUITY-05.                                                    IF1024.2
            042400     MOVE ZERO TO WS-NUM.                                         IF1024.2
            042500     MOVE  0.423434 TO MIN-RANGE.                                 IF1024.2
            042600     MOVE  0.423450 TO MAX-RANGE.                                 IF1024.2
            042700 F-ANNUITY-TEST-05.                                               IF1024.2
            042800     COMPUTE WS-NUM = FUNCTION ANNUITY(B, 4).                     IF1024.2
            042900     IF (WS-NUM >= MIN-RANGE) AND                                 IF1024.2
            043000        (WS-NUM <= MAX-RANGE) THEN                                IF1024.2
            043100                    PERFORM PASS                                  IF1024.2
            043200     ELSE                                                         IF1024.2
            043300                    MOVE WS-NUM TO COMPUTED-N                     IF1024.2
            043400                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1024.2
            043500                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1024.2
            043600                    PERFORM FAIL.                                 IF1024.2
            043700     GO TO F-ANNUITY-WRITE-05.                                    IF1024.2
            043800 F-ANNUITY-DELETE-05.                                             IF1024.2
            043900     PERFORM  DE-LETE.                                            IF1024.2
            044000     GO TO    F-ANNUITY-WRITE-05.                                 IF1024.2
            044100 F-ANNUITY-WRITE-05.                                              IF1024.2
            044200     MOVE "F-ANNUITY-05" TO PAR-NAME.                             IF1024.2
            044300     PERFORM  PRINT-DETAIL.                                       IF1024.2
            044400*****************TEST (f) - SIMPLE TEST*****************          IF1024.2
            044500 F-ANNUITY-06.                                                    IF1024.2
            044600     MOVE ZERO TO WS-NUM.                                         IF1024.2
            044700     MOVE  3.99992 TO MIN-RANGE.                                  IF1024.2
            044800     MOVE  4.00008 TO MAX-RANGE.                                  IF1024.2
            044900 F-ANNUITY-TEST-06.                                               IF1024.2
            045000     COMPUTE WS-NUM = FUNCTION ANNUITY(A, 9).                     IF1024.2
            045100     IF (WS-NUM >= MIN-RANGE) AND                                 IF1024.2
            045200        (WS-NUM <= MAX-RANGE) THEN                                IF1024.2
            045300                    PERFORM PASS                                  IF1024.2
            045400     ELSE                                                         IF1024.2
            045500                    MOVE WS-NUM TO COMPUTED-N                     IF1024.2
            045600                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1024.2
            045700                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1024.2
            045800                    PERFORM FAIL.                                 IF1024.2
            045900     GO TO F-ANNUITY-WRITE-06.                                    IF1024.2
            046000 F-ANNUITY-DELETE-06.                                             IF1024.2
            046100     PERFORM  DE-LETE.                                            IF1024.2
            046200     GO TO    F-ANNUITY-WRITE-06.                                 IF1024.2
            046300 F-ANNUITY-WRITE-06.                                              IF1024.2
            046400     MOVE "F-ANNUITY-06" TO PAR-NAME.                             IF1024.2
            046500     PERFORM  PRINT-DETAIL.                                       IF1024.2
            046600*****************TEST (g) -SIMPLE TEST******************          IF1024.2
            046700 F-ANNUITY-07.                                                    IF1024.2
            046800     MOVE ZERO TO WS-NUM.                                         IF1024.2
            046900     MOVE  5.00054 TO MIN-RANGE.                                  IF1024.2
            047000     MOVE  5.00074 TO MAX-RANGE.                                  IF1024.2
            047100 F-ANNUITY-TEST-07.                                               IF1024.2
            047200     COMPUTE WS-NUM = FUNCTION ANNUITY(5, 5).                     IF1024.2
            047300     IF (WS-NUM >= MIN-RANGE) AND                                 IF1024.2
            047400        (WS-NUM <= MAX-RANGE) THEN                                IF1024.2
            047500                    PERFORM PASS                                  IF1024.2
            047600     ELSE                                                         IF1024.2
            047700                    MOVE WS-NUM TO COMPUTED-N                     IF1024.2
            047800                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1024.2
            047900                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1024.2
            048000                    PERFORM FAIL.                                 IF1024.2
            048100     GO TO F-ANNUITY-WRITE-07.                                    IF1024.2
            048200 F-ANNUITY-DELETE-07.                                             IF1024.2
            048300     PERFORM  DE-LETE.                                            IF1024.2
            048400     GO TO    F-ANNUITY-WRITE-07.                                 IF1024.2
            048500 F-ANNUITY-WRITE-07.                                              IF1024.2
            048600     MOVE "F-ANNUITY-07" TO PAR-NAME.                             IF1024.2
            048700     PERFORM  PRINT-DETAIL.                                       IF1024.2
            048800*****************TEST (h) - SIMPLE TEST*****************          IF1024.2
            048900 F-ANNUITY-08.                                                    IF1024.2
            049000     MOVE ZERO TO WS-NUM.                                         IF1024.2
            049100     MOVE  4.03217   TO MIN-RANGE.                                IF1024.2
            049200     MOVE  4.03233 TO MAX-RANGE.                                  IF1024.2
            049300 F-ANNUITY-TEST-08.                                               IF1024.2
            049400     COMPUTE WS-NUM = FUNCTION ANNUITY(IND(1), IND(A)).           IF1024.2
            049500     IF (WS-NUM >= MIN-RANGE) AND                                 IF1024.2
            049600        (WS-NUM <= MAX-RANGE) THEN                                IF1024.2
            049700                    PERFORM PASS                                  IF1024.2
            049800     ELSE                                                         IF1024.2
            049900                    MOVE WS-NUM TO COMPUTED-N                     IF1024.2
            050000                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1024.2
            050100                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1024.2
            050200                    PERFORM FAIL.                                 IF1024.2
            050300     GO TO F-ANNUITY-WRITE-08.                                    IF1024.2
            050400 F-ANNUITY-DELETE-08.                                             IF1024.2
            050500     PERFORM  DE-LETE.                                            IF1024.2
            050600     GO TO    F-ANNUITY-WRITE-08.                                 IF1024.2
            050700 F-ANNUITY-WRITE-08.                                              IF1024.2
            050800     MOVE "F-ANNUITY-08" TO PAR-NAME.                             IF1024.2
            050900     PERFORM  PRINT-DETAIL.                                       IF1024.2
            051000*****************TEST (a) - COMPLEX TEST****************          IF1024.2
            051100 F-ANNUITY-09.                                                    IF1024.2
            051200     MOVE ZERO TO WS-NUM.                                         IF1024.2
            051300     MOVE  0.204824 TO MIN-RANGE.                                 IF1024.2
            051400     MOVE  0.204840 TO MAX-RANGE.                                 IF1024.2
            051500 F-ANNUITY-TEST-09.                                               IF1024.2
            051600     COMPUTE WS-NUM = FUNCTION ANNUITY(B / 2, 8).                 IF1024.2
            051700     IF (WS-NUM >= MIN-RANGE) AND                                 IF1024.2
            051800        (WS-NUM <= MAX-RANGE) THEN                                IF1024.2
            051900                    PERFORM PASS                                  IF1024.2
            052000     ELSE                                                         IF1024.2
            052100                    MOVE WS-NUM TO COMPUTED-N                     IF1024.2
            052200                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1024.2
            052300                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1024.2
            052400                    PERFORM FAIL.                                 IF1024.2
            052500     GO TO F-ANNUITY-WRITE-09.                                    IF1024.2
            052600 F-ANNUITY-DELETE-09.                                             IF1024.2
            052700     PERFORM  DE-LETE.                                            IF1024.2
            052800     GO TO    F-ANNUITY-WRITE-09.                                 IF1024.2
            052900 F-ANNUITY-WRITE-09.                                              IF1024.2
            053000     MOVE "F-ANNUITY-09" TO PAR-NAME.                             IF1024.2
            053100     PERFORM  PRINT-DETAIL.                                       IF1024.2
            053200*****************TEST (b) - COMPLEX TEST****************          IF1024.2
            053300 F-ANNUITY-10.                                                    IF1024.2
            053400     MOVE ZERO TO WS-NUM.                                         IF1024.2
            053500     MOVE 0.576553 TO MIN-RANGE.                                  IF1024.2
            053600     MOVE 0.576599 TO MAX-RANGE.                                  IF1024.2
            053700 F-ANNUITY-TEST-10.                                               IF1024.2
            053800     COMPUTE WS-NUM = FUNCTION ANNUITY(                           IF1024.2
            053900                      FUNCTION ANNUITY(0, 3), 3).                 IF1024.2
            054000     IF (WS-NUM >= MIN-RANGE) AND                                 IF1024.2
            054100        (WS-NUM <= MAX-RANGE) THEN                                IF1024.2
            054200                    PERFORM PASS                                  IF1024.2
            054300     ELSE                                                         IF1024.2
            054400                    MOVE WS-NUM TO COMPUTED-N                     IF1024.2
            054500                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1024.2
            054600                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1024.2
            054700                    PERFORM FAIL.                                 IF1024.2
            054800     GO TO F-ANNUITY-WRITE-10.                                    IF1024.2
            054900 F-ANNUITY-DELETE-10.                                             IF1024.2
            055000     PERFORM  DE-LETE.                                            IF1024.2
            055100     GO TO    F-ANNUITY-WRITE-10.                                 IF1024.2
            055200 F-ANNUITY-WRITE-10.                                              IF1024.2
            055300     MOVE "F-ANNUITY-10" TO PAR-NAME.                             IF1024.2
            055400     PERFORM  PRINT-DETAIL.                                       IF1024.2
            055500*****************TEST (c) - COMPLEX TEST****************          IF1024.2
            055600 F-ANNUITY-11.                                                    IF1024.2
            055700     MOVE ZERO TO WS-NUM.                                         IF1024.2
            055800     MOVE 4.49978 TO MIN-RANGE.                                   IF1024.2
            055900     MOVE 5.50022 TO MAX-RANGE.                                   IF1024.2
            056000 F-ANNUITY-TEST-11.                                               IF1024.2
            056100     COMPUTE WS-NUM = FUNCTION ANNUITY(0, 2) + 5.                 IF1024.2
            056200     IF (WS-NUM >= MIN-RANGE) AND                                 IF1024.2
            056300        (WS-NUM <= MAX-RANGE) THEN                                IF1024.2
            056400                    PERFORM PASS                                  IF1024.2
            056500     ELSE                                                         IF1024.2
            056600                    MOVE WS-NUM TO COMPUTED-N                     IF1024.2
            056700                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1024.2
            056800                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1024.2
            056900                    PERFORM FAIL.                                 IF1024.2
            057000     GO TO F-ANNUITY-WRITE-11.                                    IF1024.2
            057100 F-ANNUITY-DELETE-11.                                             IF1024.2
            057200     PERFORM  DE-LETE.                                            IF1024.2
            057300     GO TO    F-ANNUITY-WRITE-11.                                 IF1024.2
            057400 F-ANNUITY-WRITE-11.                                              IF1024.2
            057500     MOVE "F-ANNUITY-11" TO PAR-NAME.                             IF1024.2
            057600     PERFORM  PRINT-DETAIL.                                       IF1024.2
            057700*****************TEST (d) - COMPLEX TEST****************          IF1024.2
            057800 F-ANNUITY-12.                                                    IF1024.2
            057900     MOVE ZERO TO WS-NUM.                                         IF1024.2
            058000     MOVE 0.999960 TO MIN-RANGE.                                  IF1024.2
            058100     MOVE 1.00004 TO MAX-RANGE.                                   IF1024.2
            058200 F-ANNUITY-TEST-12.                                               IF1024.2
            058300     COMPUTE WS-NUM = FUNCTION ANNUITY(0, 2) +                    IF1024.2
            058400                      FUNCTION ANNUITY(0, 2).                     IF1024.2
            058500     IF (WS-NUM >= MIN-RANGE) AND                                 IF1024.2
            058600        (WS-NUM <= MAX-RANGE) THEN                                IF1024.2
            058700                    PERFORM PASS                                  IF1024.2
            058800     ELSE                                                         IF1024.2
            058900                    MOVE WS-NUM TO COMPUTED-N                     IF1024.2
            059000                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1024.2
            059100                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1024.2
            059200                    PERFORM FAIL.                                 IF1024.2
            059300     GO TO F-ANNUITY-WRITE-12.                                    IF1024.2
            059400 F-ANNUITY-DELETE-12.                                             IF1024.2
            059500     PERFORM  DE-LETE.                                            IF1024.2
            059600     GO TO    F-ANNUITY-WRITE-12.                                 IF1024.2
            059700 F-ANNUITY-WRITE-12.                                              IF1024.2
            059800     MOVE "F-ANNUITY-12" TO PAR-NAME.                             IF1024.2
            059900     PERFORM  PRINT-DETAIL.                                       IF1024.2
            060000*****************SPECIAL PERFORM TEST**********************       IF1024.2
            060100 F-ANNUITY-13.                                                    IF1024.2
            060200     PERFORM F-ANNUITY-TEST-13                                    IF1024.2
            060300       UNTIL FUNCTION ANNUITY(0, ARG2) < .25.                     IF1024.2
            060400     PERFORM PASS.                                                IF1024.2
            060500     GO TO F-ANNUITY-WRITE-13.                                    IF1024.2
            060600 F-ANNUITY-TEST-13.                                               IF1024.2
            060700     COMPUTE ARG2 = ARG2 + 1.                                     IF1024.2
            060800 F-ANNUITY-DELETE-13.                                             IF1024.2
            060900     PERFORM  DE-LETE.                                            IF1024.2
            061000     GO TO    F-ANNUITY-WRITE-13.                                 IF1024.2
            061100 F-ANNUITY-WRITE-13.                                              IF1024.2
            061200     MOVE "F-ANNUITY-13" TO PAR-NAME.                             IF1024.2
            061300     PERFORM  PRINT-DETAIL.                                       IF1024.2
            061400********************END OF TESTS***************                   IF1024.2
            061500 CCVS-EXIT SECTION.                                               IF1024.2
            061600 CCVS-999999.                                                     IF1024.2
            061700     GO TO CLOSE-FILES.                                           IF1024.2
                  *END-OF,IF102A                                                                  
        """)
    )

    @Test
    fun if1034_2() = rewriteRun(
        cobol("""
                  *HEADER,COBOL,IF103A                                                            
            000100 IDENTIFICATION DIVISION.                                         IF1034.2
            000200 PROGRAM-ID.                                                      IF1034.2
            000300     IF103A.                                                      IF1034.2
            000400                                                                  IF1034.2
            000500***********************************************************       IF1034.2
            000600*                                                         *       IF1034.2
            000700*   This program is intended to form part of the CCVS85   *       IF1034.2
            000800*   COBOL Test Suite. It contains tests for the           *       IF1034.2
            000900*   Intrinsic Function ASIN.                              *       IF1034.2
            001000*                                                         *       IF1034.2
            001100***********************************************************       IF1034.2
            001200 ENVIRONMENT DIVISION.                                            IF1034.2
            001300 CONFIGURATION SECTION.                                           IF1034.2
            001400 SOURCE-COMPUTER.                                                 IF1034.2
            001500     XXXXX082.                                                    IF1034.2
            001600 OBJECT-COMPUTER.                                                 IF1034.2
            001700     XXXXX083.                                                    IF1034.2
            001800 INPUT-OUTPUT SECTION.                                            IF1034.2
            001900 FILE-CONTROL.                                                    IF1034.2
            002000     SELECT PRINT-FILE ASSIGN TO                                  IF1034.2
            002100     XXXXX055.                                                    IF1034.2
            002200 DATA DIVISION.                                                   IF1034.2
            002300 FILE SECTION.                                                    IF1034.2
            002400 FD  PRINT-FILE.                                                  IF1034.2
            002500 01  PRINT-REC PICTURE X(120).                                    IF1034.2
            002600 01  DUMMY-RECORD PICTURE X(120).                                 IF1034.2
            002700 WORKING-STORAGE SECTION.                                         IF1034.2
            002800***********************************************************       IF1034.2
            002900* Variables specific to the Intrinsic Function Test IF103A*       IF1034.2
            003000***********************************************************       IF1034.2
            003100 01  A                   PIC S9(5)V9(5)      VALUE -0.00004.      IF1034.2
            003200 01  B                   PIC S9(10)          VALUE 2.             IF1034.2
            003300 01  C                   PIC S9(10)          VALUE 100000.        IF1034.2
            003400 01  D                   PIC S9(10)          VALUE 1000.          IF1034.2
            003500 01  PI                  PIC S9V9(17)        VALUE 3.141592654.   IF1034.2
            003600 01  ARG1                PIC S9V9(17)        VALUE 1.             IF1034.2
            003700 01  SQRT2               PIC S9V9(17)        VALUE 1.414213562.   IF1034.2
            003800 01  SQRT3D2             PIC S9V9(17)        VALUE 0.866025403.   IF1034.2
            003900 01  ARR                                     VALUE "40537".       IF1034.2
            004000     02  IND OCCURS 5 TIMES PIC 9.                                IF1034.2
            004100 01  TEMP                PIC S9(5)V9(5).                          IF1034.2
            004200 01  WS-NUM              PIC S9(5)V9(6).                          IF1034.2
            004300 01  MIN-RANGE           PIC S9(5)V9(7).                          IF1034.2
            004400 01  MAX-RANGE           PIC S9(5)V9(7).                          IF1034.2
            004500*                                                                 IF1034.2
            004600**********************************************************        IF1034.2
            004700*                                                                 IF1034.2
            004800 01  TEST-RESULTS.                                                IF1034.2
            004900     02 FILLER                   PIC X      VALUE SPACE.          IF1034.2
            005000     02 FEATURE                  PIC X(20)  VALUE SPACE.          IF1034.2
            005100     02 FILLER                   PIC X      VALUE SPACE.          IF1034.2
            005200     02 P-OR-F                   PIC X(5)   VALUE SPACE.          IF1034.2
            005300     02 FILLER                   PIC X      VALUE SPACE.          IF1034.2
            005400     02  PAR-NAME.                                                IF1034.2
            005500       03 FILLER                 PIC X(19)  VALUE SPACE.          IF1034.2
            005600       03  PARDOT-X              PIC X      VALUE SPACE.          IF1034.2
            005700       03 DOTVALUE               PIC 99     VALUE ZERO.           IF1034.2
            005800     02 FILLER                   PIC X(8)   VALUE SPACE.          IF1034.2
            005900     02 RE-MARK                  PIC X(61).                       IF1034.2
            006000 01  TEST-COMPUTED.                                               IF1034.2
            006100     02 FILLER                   PIC X(30)  VALUE SPACE.          IF1034.2
            006200     02 FILLER                   PIC X(17)  VALUE                 IF1034.2
            006300            "       COMPUTED=".                                   IF1034.2
            006400     02 COMPUTED-X.                                               IF1034.2
            006500     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          IF1034.2
            006600     03 COMPUTED-N               REDEFINES COMPUTED-A             IF1034.2
            006700                                 PIC -9(9).9(9).                  IF1034.2
            006800     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         IF1034.2
            006900     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     IF1034.2
            007000     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     IF1034.2
            007100     03       CM-18V0 REDEFINES COMPUTED-A.                       IF1034.2
            007200         04 COMPUTED-18V0                    PIC -9(18).          IF1034.2
            007300         04 FILLER                           PIC X.               IF1034.2
            007400     03 FILLER PIC X(50) VALUE SPACE.                             IF1034.2
            007500 01  TEST-CORRECT.                                                IF1034.2
            007600     02 FILLER PIC X(30) VALUE SPACE.                             IF1034.2
            007700     02 FILLER PIC X(17) VALUE "       CORRECT =".                IF1034.2
            007800     02 CORRECT-X.                                                IF1034.2
            007900     03 CORRECT-A                  PIC X(20) VALUE SPACE.         IF1034.2
            008000     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      IF1034.2
            008100     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         IF1034.2
            008200     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     IF1034.2
            008300     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     IF1034.2
            008400     03      CR-18V0 REDEFINES CORRECT-A.                         IF1034.2
            008500         04 CORRECT-18V0                     PIC -9(18).          IF1034.2
            008600         04 FILLER                           PIC X.               IF1034.2
            008700     03 FILLER PIC X(2) VALUE SPACE.                              IF1034.2
            008800     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     IF1034.2
            008900 01  TEST-CORRECT-MIN.                                            IF1034.2
            009000     02 FILLER PIC X(30) VALUE SPACE.                             IF1034.2
            009100     02 FILLER PIC X(17) VALUE "     MIN VALUE =".                IF1034.2
            009200     02 CORRECTMI-X.                                              IF1034.2
            009300     03 CORRECTMI-A                 PIC X(20) VALUE SPACE.        IF1034.2
            009400     03 CORRECT-MIN    REDEFINES CORRECTMI-A     PIC -9(9).9(9).  IF1034.2
            009500     03 CORRECTMI-0V18 REDEFINES CORRECTMI-A     PIC -.9(18).     IF1034.2
            009600     03 CORRECTMI-4V14 REDEFINES CORRECTMI-A     PIC -9(4).9(14). IF1034.2
            009700     03 CORRECTMI-14V4 REDEFINES CORRECTMI-A     PIC -9(14).9(4). IF1034.2
            009800     03      CR-18V0 REDEFINES CORRECTMI-A.                       IF1034.2
            009900         04 CORRECTMI-18V0                     PIC -9(18).        IF1034.2
            010000         04 FILLER                           PIC X.               IF1034.2
            010100     03 FILLER PIC X(2) VALUE SPACE.                              IF1034.2
            010200     03 FILLER                           PIC X(48) VALUE SPACE.   IF1034.2
            010300 01  TEST-CORRECT-MAX.                                            IF1034.2
            010400     02 FILLER PIC X(30) VALUE SPACE.                             IF1034.2
            010500     02 FILLER PIC X(17) VALUE "     MAX VALUE =".                IF1034.2
            010600     02 CORRECTMA-X.                                              IF1034.2
            010700     03 CORRECTMA-A                  PIC X(20) VALUE SPACE.       IF1034.2
            010800     03 CORRECT-MAX    REDEFINES CORRECTMA-A     PIC -9(9).9(9).  IF1034.2
            010900     03 CORRECTMA-0V18 REDEFINES CORRECTMA-A     PIC -.9(18).     IF1034.2
            011000     03 CORRECTMA-4V14 REDEFINES CORRECTMA-A     PIC -9(4).9(14). IF1034.2
            011100     03 CORRECTMA-14V4 REDEFINES CORRECTMA-A     PIC -9(14).9(4). IF1034.2
            011200     03      CR-18V0 REDEFINES CORRECTMA-A.                       IF1034.2
            011300         04 CORRECTMA-18V0                     PIC -9(18).        IF1034.2
            011400         04 FILLER                           PIC X.               IF1034.2
            011500     03 FILLER PIC X(2) VALUE SPACE.                              IF1034.2
            011600     03 CORMA-ANSI-REFERENCE             PIC X(48) VALUE SPACE.   IF1034.2
            011700 01  CCVS-C-1.                                                    IF1034.2
            011800     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PAIF1034.2
            011900-    "SS  PARAGRAPH-NAME                                          IF1034.2
            012000-    "       REMARKS".                                            IF1034.2
            012100     02 FILLER                     PIC X(20)    VALUE SPACE.      IF1034.2
            012200 01  CCVS-C-2.                                                    IF1034.2
            012300     02 FILLER                     PIC X        VALUE SPACE.      IF1034.2
            012400     02 FILLER                     PIC X(6)     VALUE "TESTED".   IF1034.2
            012500     02 FILLER                     PIC X(15)    VALUE SPACE.      IF1034.2
            012600     02 FILLER                     PIC X(4)     VALUE "FAIL".     IF1034.2
            012700     02 FILLER                     PIC X(94)    VALUE SPACE.      IF1034.2
            012800 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       IF1034.2
            012900 01  REC-CT                        PIC 99       VALUE ZERO.       IF1034.2
            013000 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       IF1034.2
            013100 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       IF1034.2
            013200 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       IF1034.2
            013300 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       IF1034.2
            013400 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       IF1034.2
            013500 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       IF1034.2
            013600 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      IF1034.2
            013700 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       IF1034.2
            013800 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     IF1034.2
            013900 01  CCVS-H-1.                                                    IF1034.2
            014000     02  FILLER                    PIC X(39)    VALUE SPACES.     IF1034.2
            014100     02  FILLER                    PIC X(42)    VALUE             IF1034.2
            014200     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 IF1034.2
            014300     02  FILLER                    PIC X(39)    VALUE SPACES.     IF1034.2
            014400 01  CCVS-H-2A.                                                   IF1034.2
            014500   02  FILLER                        PIC X(40)  VALUE SPACE.      IF1034.2
            014600   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  IF1034.2
            014700   02  FILLER                        PIC XXXX   VALUE             IF1034.2
            014800     "4.2 ".                                                      IF1034.2
            014900   02  FILLER                        PIC X(28)  VALUE             IF1034.2
            015000            " COPY - NOT FOR DISTRIBUTION".                       IF1034.2
            015100   02  FILLER                        PIC X(41)  VALUE SPACE.      IF1034.2
            015200                                                                  IF1034.2
            015300 01  CCVS-H-2B.                                                   IF1034.2
            015400   02  FILLER                        PIC X(15)  VALUE             IF1034.2
            015500            "TEST RESULT OF ".                                    IF1034.2
            015600   02  TEST-ID                       PIC X(9).                    IF1034.2
            015700   02  FILLER                        PIC X(4)   VALUE             IF1034.2
            015800            " IN ".                                               IF1034.2
            015900   02  FILLER                        PIC X(12)  VALUE             IF1034.2
            016000     " HIGH       ".                                              IF1034.2
            016100   02  FILLER                        PIC X(22)  VALUE             IF1034.2
            016200            " LEVEL VALIDATION FOR ".                             IF1034.2
            016300   02  FILLER                        PIC X(58)  VALUE             IF1034.2
            016400     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".IF1034.2
            016500 01  CCVS-H-3.                                                    IF1034.2
            016600     02  FILLER                      PIC X(34)  VALUE             IF1034.2
            016700            " FOR OFFICIAL USE ONLY    ".                         IF1034.2
            016800     02  FILLER                      PIC X(58)  VALUE             IF1034.2
            016900     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".IF1034.2
            017000     02  FILLER                      PIC X(28)  VALUE             IF1034.2
            017100            "  COPYRIGHT   1985 ".                                IF1034.2
            017200 01  CCVS-E-1.                                                    IF1034.2
            017300     02 FILLER                       PIC X(52)  VALUE SPACE.      IF1034.2
            017400     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              IF1034.2
            017500     02 ID-AGAIN                     PIC X(9).                    IF1034.2
            017600     02 FILLER                       PIC X(45)  VALUE SPACES.     IF1034.2
            017700 01  CCVS-E-2.                                                    IF1034.2
            017800     02  FILLER                      PIC X(31)  VALUE SPACE.      IF1034.2
            017900     02  FILLER                      PIC X(21)  VALUE SPACE.      IF1034.2
            018000     02 CCVS-E-2-2.                                               IF1034.2
            018100         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      IF1034.2
            018200         03 FILLER                   PIC X      VALUE SPACE.      IF1034.2
            018300         03 ENDER-DESC               PIC X(44)  VALUE             IF1034.2
            018400            "ERRORS ENCOUNTERED".                                 IF1034.2
            018500 01  CCVS-E-3.                                                    IF1034.2
            018600     02  FILLER                      PIC X(22)  VALUE             IF1034.2
            018700            " FOR OFFICIAL USE ONLY".                             IF1034.2
            018800     02  FILLER                      PIC X(12)  VALUE SPACE.      IF1034.2
            018900     02  FILLER                      PIC X(58)  VALUE             IF1034.2
            019000     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".IF1034.2
            019100     02  FILLER                      PIC X(13)  VALUE SPACE.      IF1034.2
            019200     02 FILLER                       PIC X(15)  VALUE             IF1034.2
            019300             " COPYRIGHT 1985".                                   IF1034.2
            019400 01  CCVS-E-4.                                                    IF1034.2
            019500     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      IF1034.2
            019600     02 FILLER                       PIC X(4)   VALUE " OF ".     IF1034.2
            019700     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      IF1034.2
            019800     02 FILLER                       PIC X(40)  VALUE             IF1034.2
            019900      "  TESTS WERE EXECUTED SUCCESSFULLY".                       IF1034.2
            020000 01  XXINFO.                                                      IF1034.2
            020100     02 FILLER                       PIC X(19)  VALUE             IF1034.2
            020200            "*** INFORMATION ***".                                IF1034.2
            020300     02 INFO-TEXT.                                                IF1034.2
            020400       04 FILLER                     PIC X(8)   VALUE SPACE.      IF1034.2
            020500       04 XXCOMPUTED                 PIC X(20).                   IF1034.2
            020600       04 FILLER                     PIC X(5)   VALUE SPACE.      IF1034.2
            020700       04 XXCORRECT                  PIC X(20).                   IF1034.2
            020800     02 INF-ANSI-REFERENCE           PIC X(48).                   IF1034.2
            020900 01  HYPHEN-LINE.                                                 IF1034.2
            021000     02 FILLER  PIC IS X VALUE IS SPACE.                          IF1034.2
            021100     02 FILLER  PIC IS X(65)    VALUE IS "************************IF1034.2
            021200-    "*****************************************".                 IF1034.2
            021300     02 FILLER  PIC IS X(54)    VALUE IS "************************IF1034.2
            021400-    "******************************".                            IF1034.2
            021500 01  CCVS-PGM-ID                     PIC X(9)   VALUE             IF1034.2
            021600     "IF103A".                                                    IF1034.2
            021700 PROCEDURE DIVISION.                                              IF1034.2
            021800 CCVS1 SECTION.                                                   IF1034.2
            021900 OPEN-FILES.                                                      IF1034.2
            022000     OPEN     OUTPUT PRINT-FILE.                                  IF1034.2
            022100     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   IF1034.2
            022200     MOVE    SPACE TO TEST-RESULTS.                               IF1034.2
            022300     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             IF1034.2
            022400     GO TO CCVS1-EXIT.                                            IF1034.2
            022500 CLOSE-FILES.                                                     IF1034.2
            022600     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   IF1034.2
            022700 TERMINATE-CCVS.                                                  IF1034.2
            022800     STOP     RUN.                                                IF1034.2
            022900 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         IF1034.2
            023000 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           IF1034.2
            023100 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          IF1034.2
            023200 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      IF1034.2
            023300     MOVE "****TEST DELETED****" TO RE-MARK.                      IF1034.2
            023400 PRINT-DETAIL.                                                    IF1034.2
            023500     IF REC-CT NOT EQUAL TO ZERO                                  IF1034.2
            023600             MOVE "." TO PARDOT-X                                 IF1034.2
            023700             MOVE REC-CT TO DOTVALUE.                             IF1034.2
            023800     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      IF1034.2
            023900     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               IF1034.2
            024000        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 IF1034.2
            024100          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 IF1034.2
            024200     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              IF1034.2
            024300     MOVE SPACE TO CORRECT-X.                                     IF1034.2
            024400     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         IF1034.2
            024500     MOVE     SPACE TO RE-MARK.                                   IF1034.2
            024600 HEAD-ROUTINE.                                                    IF1034.2
            024700     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  IF1034.2
            024800     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  IF1034.2
            024900     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  IF1034.2
            025000     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  IF1034.2
            025100 COLUMN-NAMES-ROUTINE.                                            IF1034.2
            025200     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           IF1034.2
            025300     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1034.2
            025400     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        IF1034.2
            025500 END-ROUTINE.                                                     IF1034.2
            025600     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.IF1034.2
            025700 END-RTN-EXIT.                                                    IF1034.2
            025800     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1034.2
            025900 END-ROUTINE-1.                                                   IF1034.2
            026000      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      IF1034.2
            026100      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               IF1034.2
            026200      ADD PASS-COUNTER TO ERROR-HOLD.                             IF1034.2
            026300      MOVE PASS-COUNTER TO CCVS-E-4-1.                            IF1034.2
            026400      MOVE ERROR-HOLD TO CCVS-E-4-2.                              IF1034.2
            026500      MOVE CCVS-E-4 TO CCVS-E-2-2.                                IF1034.2
            026600      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           IF1034.2
            026700  END-ROUTINE-12.                                                 IF1034.2
            026800      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        IF1034.2
            026900     IF       ERROR-COUNTER IS EQUAL TO ZERO                      IF1034.2
            027000         MOVE "NO " TO ERROR-TOTAL                                IF1034.2
            027100         ELSE                                                     IF1034.2
            027200         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       IF1034.2
            027300     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           IF1034.2
            027400     PERFORM WRITE-LINE.                                          IF1034.2
            027500 END-ROUTINE-13.                                                  IF1034.2
            027600     IF DELETE-COUNTER IS EQUAL TO ZERO                           IF1034.2
            027700         MOVE "NO " TO ERROR-TOTAL  ELSE                          IF1034.2
            027800         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      IF1034.2
            027900     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   IF1034.2
            028000     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           IF1034.2
            028100      IF   INSPECT-COUNTER EQUAL TO ZERO                          IF1034.2
            028200          MOVE "NO " TO ERROR-TOTAL                               IF1034.2
            028300      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   IF1034.2
            028400      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            IF1034.2
            028500      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          IF1034.2
            028600     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           IF1034.2
            028700 WRITE-LINE.                                                      IF1034.2
            028800     ADD 1 TO RECORD-COUNT.                                       IF1034.2
            028900*    IF RECORD-COUNT GREATER 42                                   IF1034.2
            029000*        MOVE DUMMY-RECORD TO DUMMY-HOLD                          IF1034.2
            029100*        MOVE SPACE TO DUMMY-RECORD                               IF1034.2
            029200*        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  IF1034.2
            029300*        MOVE CCVS-H-1  TO DUMMY-RECORD  PERFORM WRT-LN 2 TIMES   IF1034.2
            029400*        MOVE CCVS-H-2A TO DUMMY-RECORD  PERFORM WRT-LN 2 TIMES   IF1034.2
            029500*        MOVE CCVS-H-2B TO DUMMY-RECORD  PERFORM WRT-LN 3 TIMES   IF1034.2
            029600*        MOVE CCVS-H-3  TO DUMMY-RECORD  PERFORM WRT-LN 3 TIMES   IF1034.2
            029700*        MOVE CCVS-C-1  TO DUMMY-RECORD  PERFORM WRT-LN           IF1034.2
            029800*        MOVE CCVS-C-2  TO DUMMY-RECORD  PERFORM WRT-LN           IF1034.2
            029900*        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          IF1034.2
            030000*        MOVE DUMMY-HOLD TO DUMMY-RECORD                          IF1034.2
            030100*        MOVE ZERO TO RECORD-COUNT.                               IF1034.2
            030200     PERFORM WRT-LN.                                              IF1034.2
            030300 WRT-LN.                                                          IF1034.2
            030400     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               IF1034.2
            030500     MOVE SPACE TO DUMMY-RECORD.                                  IF1034.2
            030600 BLANK-LINE-PRINT.                                                IF1034.2
            030700     PERFORM WRT-LN.                                              IF1034.2
            030800 FAIL-ROUTINE.                                                    IF1034.2
            030900     IF     COMPUTED-X NOT EQUAL TO SPACE                         IF1034.2
            031000            GO TO FAIL-ROUTINE-WRITE.                             IF1034.2
            031100     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.IF1034.2
            031200     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 IF1034.2
            031300     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   IF1034.2
            031400     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1034.2
            031500     MOVE   SPACES TO INF-ANSI-REFERENCE.                         IF1034.2
            031600     GO TO  FAIL-ROUTINE-EX.                                      IF1034.2
            031700 FAIL-ROUTINE-WRITE.                                              IF1034.2
            031800     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE.        IF1034.2
            031900     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE                  IF1034.2
            032000                              CORMA-ANSI-REFERENCE.               IF1034.2
            032100     IF CORRECT-MIN NOT EQUAL TO SPACES THEN                      IF1034.2
            032200           MOVE TEST-CORRECT-MIN TO PRINT-REC PERFORM WRITE-LINE  IF1034.2
            032300           MOVE TEST-CORRECT-MAX TO PRINT-REC PERFORM WRITE-LINE  IF1034.2
            032400     ELSE                                                         IF1034.2
            032500           MOVE TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE.     IF1034.2
            032600     PERFORM WRITE-LINE.                                          IF1034.2
            032700     MOVE   SPACES TO COR-ANSI-REFERENCE.                         IF1034.2
            032800 FAIL-ROUTINE-EX. EXIT.                                           IF1034.2
            032900 BAIL-OUT.                                                        IF1034.2
            033000     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   IF1034.2
            033100     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           IF1034.2
            033200 BAIL-OUT-WRITE.                                                  IF1034.2
            033300     MOVE CORRECT-A TO XXCORRECT.                                 IF1034.2
            033400     MOVE COMPUTED-A TO XXCOMPUTED.                               IF1034.2
            033500     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 IF1034.2
            033600     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1034.2
            033700     MOVE   SPACES TO INF-ANSI-REFERENCE.                         IF1034.2
            033800 BAIL-OUT-EX. EXIT.                                               IF1034.2
            033900 CCVS1-EXIT.                                                      IF1034.2
            034000     EXIT.                                                        IF1034.2
            034100********************************************************          IF1034.2
            034200*                                                      *          IF1034.2
            034300*    Intrinsic Function Tests         IF103A - ASIN    *          IF1034.2
            034400*                                                      *          IF1034.2
            034500********************************************************          IF1034.2
            034600 SECT-IF103A SECTION.                                             IF1034.2
            034700 F-ASIN-INFO.                                                     IF1034.2
            034800     MOVE     "See ref. A-35 2.7" TO ANSI-REFERENCE.              IF1034.2
            034900     MOVE     "ASIN Function" TO FEATURE.                         IF1034.2
            035000*****************TEST (a) - SIMPLE TEST*****************          IF1034.2
            035100 F-ASIN-01.                                                       IF1034.2
            035200     MOVE ZERO TO WS-NUM.                                         IF1034.2
            035300     MOVE  1.57076 TO MIN-RANGE.                                  IF1034.2
            035400     MOVE  1.57080 TO MAX-RANGE.                                  IF1034.2
            035500 F-ASIN-TEST-01.                                                  IF1034.2
            035600     COMPUTE WS-NUM = FUNCTION ASIN(1.0).                         IF1034.2
            035700     IF (WS-NUM >= MIN-RANGE) AND                                 IF1034.2
            035800        (WS-NUM <= MAX-RANGE) THEN                                IF1034.2
            035900                    PERFORM PASS                                  IF1034.2
            036000     ELSE                                                         IF1034.2
            036100                    MOVE WS-NUM TO COMPUTED-N                     IF1034.2
            036200                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1034.2
            036300                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1034.2
            036400                    PERFORM FAIL.                                 IF1034.2
            036500     GO TO F-ASIN-WRITE-01.                                       IF1034.2
            036600 F-ASIN-DELETE-01.                                                IF1034.2
            036700     PERFORM  DE-LETE.                                            IF1034.2
            036800     GO TO    F-ASIN-WRITE-01.                                    IF1034.2
            036900 F-ASIN-WRITE-01.                                                 IF1034.2
            037000     MOVE "F-ASIN-01" TO PAR-NAME.                                IF1034.2
            037100     PERFORM  PRINT-DETAIL.                                       IF1034.2
            037200*****************TEST (b) - SIMPLE TEST*****************          IF1034.2
            037300 F-ASIN-02.                                                       IF1034.2
            037400     EVALUATE FUNCTION ASIN(0.5)                                  IF1034.2
            037500     WHEN  0.523588 THRU 0.523609                                 IF1034.2
            037600               PERFORM PASS                                       IF1034.2
            037700     WHEN OTHER                                                   IF1034.2
            037800               PERFORM FAIL.                                      IF1034.2
            037900     GO TO F-ASIN-WRITE-02.                                       IF1034.2
            038000 F-ASIN-DELETE-02.                                                IF1034.2
            038100     PERFORM  DE-LETE.                                            IF1034.2
            038200     GO TO    F-ASIN-WRITE-02.                                    IF1034.2
            038300 F-ASIN-WRITE-02.                                                 IF1034.2
            038400     MOVE "F-ASIN-02" TO PAR-NAME.                                IF1034.2
            038500     PERFORM  PRINT-DETAIL.                                       IF1034.2
            038600*****************TEST (c) - SIMPLE TEST*****************          IF1034.2
            038700 F-ASIN-03.                                                       IF1034.2
            038800     MOVE -0.000020 TO MIN-RANGE.                                 IF1034.2
            038900     MOVE  0.000020 TO MAX-RANGE.                                 IF1034.2
            039000 F-ASIN-TEST-03.                                                  IF1034.2
            039100     IF (FUNCTION ASIN(0) >= MIN-RANGE) AND                       IF1034.2
            039200        (FUNCTION ASIN(0) <= MAX-RANGE) THEN                      IF1034.2
            039300                    PERFORM PASS                                  IF1034.2
            039400     ELSE                                                         IF1034.2
            039500                    PERFORM FAIL.                                 IF1034.2
            039600     GO TO F-ASIN-WRITE-03.                                       IF1034.2
            039700 F-ASIN-DELETE-03.                                                IF1034.2
            039800     PERFORM  DE-LETE.                                            IF1034.2
            039900     GO TO    F-ASIN-WRITE-03.                                    IF1034.2
            040000 F-ASIN-WRITE-03.                                                 IF1034.2
            040100     MOVE "F-ASIN-03" TO PAR-NAME.                                IF1034.2
            040200     PERFORM  PRINT-DETAIL.                                       IF1034.2
            040300*****************TEST (d) - SIMPLE TEST*****************          IF1034.2
            040400 F-ASIN-04.                                                       IF1034.2
            040500     MOVE ZERO TO WS-NUM.                                         IF1034.2
            040600     MOVE -1.57080 TO MIN-RANGE.                                  IF1034.2
            040700     MOVE -1.57076 TO MAX-RANGE.                                  IF1034.2
            040800 F-ASIN-TEST-04.                                                  IF1034.2
            040900     COMPUTE WS-NUM = FUNCTION ASIN(-1).                          IF1034.2
            041000     IF (WS-NUM >= MIN-RANGE) AND                                 IF1034.2
            041100        (WS-NUM <= MAX-RANGE) THEN                                IF1034.2
            041200                    PERFORM PASS                                  IF1034.2
            041300     ELSE                                                         IF1034.2
            041400                    MOVE WS-NUM TO COMPUTED-N                     IF1034.2
            041500                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1034.2
            041600                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1034.2
            041700                    PERFORM FAIL.                                 IF1034.2
            041800     GO TO F-ASIN-WRITE-04.                                       IF1034.2
            041900 F-ASIN-DELETE-04.                                                IF1034.2
            042000     PERFORM  DE-LETE.                                            IF1034.2
            042100     GO TO    F-ASIN-WRITE-04.                                    IF1034.2
            042200 F-ASIN-WRITE-04.                                                 IF1034.2
            042300     MOVE "F-ASIN-04" TO PAR-NAME.                                IF1034.2
            042400     PERFORM  PRINT-DETAIL.                                       IF1034.2
            042500*****************TEST (e) - SIMPLE TEST*****************          IF1034.2
            042600 F-ASIN-05.                                                       IF1034.2
            042700     MOVE ZERO TO WS-NUM.                                         IF1034.2
            042800     MOVE  1.52604 TO MIN-RANGE.                                  IF1034.2
            042900     MOVE  1.52610 TO MAX-RANGE.                                  IF1034.2
            043000 F-ASIN-TEST-05.                                                  IF1034.2
            043100     COMPUTE WS-NUM = FUNCTION ASIN(.999).                        IF1034.2
            043200     IF (WS-NUM >= MIN-RANGE) AND                                 IF1034.2
            043300        (WS-NUM <= MAX-RANGE) THEN                                IF1034.2
            043400                    PERFORM PASS                                  IF1034.2
            043500     ELSE                                                         IF1034.2
            043600                    MOVE WS-NUM TO COMPUTED-N                     IF1034.2
            043700                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1034.2
            043800                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1034.2
            043900                    PERFORM FAIL.                                 IF1034.2
            044000     GO TO F-ASIN-WRITE-05.                                       IF1034.2
            044100 F-ASIN-DELETE-05.                                                IF1034.2
            044200     PERFORM  DE-LETE.                                            IF1034.2
            044300     GO TO    F-ASIN-WRITE-05.                                    IF1034.2
            044400 F-ASIN-WRITE-05.                                                 IF1034.2
            044500     MOVE "F-ASIN-05" TO PAR-NAME.                                IF1034.2
            044600     PERFORM  PRINT-DETAIL.                                       IF1034.2
            044700*****************TEST (f) - SIMPLE TEST*****************          IF1034.2
            044800 F-ASIN-06.                                                       IF1034.2
            044900     MOVE ZERO TO WS-NUM.                                         IF1034.2
            045000     MOVE  0.512079 TO MIN-RANGE.                                 IF1034.2
            045100     MOVE  0.512099 TO MAX-RANGE.                                 IF1034.2
            045200 F-ASIN-TEST-06.                                                  IF1034.2
            045300     COMPUTE WS-NUM = FUNCTION ASIN(.49).                         IF1034.2
            045400     IF (WS-NUM >= MIN-RANGE) AND                                 IF1034.2
            045500        (WS-NUM <= MAX-RANGE) THEN                                IF1034.2
            045600                    PERFORM PASS                                  IF1034.2
            045700     ELSE                                                         IF1034.2
            045800                    MOVE WS-NUM TO COMPUTED-N                     IF1034.2
            045900                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1034.2
            046000                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1034.2
            046100                    PERFORM FAIL.                                 IF1034.2
            046200     GO TO F-ASIN-WRITE-06.                                       IF1034.2
            046300 F-ASIN-DELETE-06.                                                IF1034.2
            046400     PERFORM  DE-LETE.                                            IF1034.2
            046500     GO TO    F-ASIN-WRITE-06.                                    IF1034.2
            046600 F-ASIN-WRITE-06.                                                 IF1034.2
            046700     MOVE "F-ASIN-06" TO PAR-NAME.                                IF1034.2
            046800     PERFORM  PRINT-DETAIL.                                       IF1034.2
            046900*****************TEST (h) - SIMPLE TEST*****************          IF1034.2
            047000 F-ASIN-08.                                                       IF1034.2
            047100     MOVE ZERO TO WS-NUM.                                         IF1034.2
            047200     MOVE -1.52610 TO MIN-RANGE.                                  IF1034.2
            047300     MOVE -1.52604 TO MAX-RANGE.                                  IF1034.2
            047400 F-ASIN-TEST-08.                                                  IF1034.2
            047500     COMPUTE WS-NUM = FUNCTION ASIN(-.999).                       IF1034.2
            047600     IF (WS-NUM >= MIN-RANGE) AND                                 IF1034.2
            047700        (WS-NUM <= MAX-RANGE) THEN                                IF1034.2
            047800                    PERFORM PASS                                  IF1034.2
            047900     ELSE                                                         IF1034.2
            048000                    MOVE WS-NUM TO COMPUTED-N                     IF1034.2
            048100                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1034.2
            048200                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1034.2
            048300                    PERFORM FAIL.                                 IF1034.2
            048400     GO TO F-ASIN-WRITE-08.                                       IF1034.2
            048500 F-ASIN-DELETE-08.                                                IF1034.2
            048600     PERFORM  DE-LETE.                                            IF1034.2
            048700     GO TO    F-ASIN-WRITE-08.                                    IF1034.2
            048800 F-ASIN-WRITE-08.                                                 IF1034.2
            048900     MOVE "F-ASIN-08" TO PAR-NAME.                                IF1034.2
            049000     PERFORM  PRINT-DETAIL.                                       IF1034.2
            049100*****************TEST (k) - SIMPLE TEST*****************          IF1034.2
            049200 F-ASIN-11.                                                       IF1034.2
            049300     MOVE ZERO TO WS-NUM.                                         IF1034.2
            049400     MOVE -0.000020 TO MIN-RANGE.                                 IF1034.2
            049500     MOVE  0.000020 TO MAX-RANGE.                                 IF1034.2
            049600 F-ASIN-TEST-11.                                                  IF1034.2
            049700     COMPUTE WS-NUM = FUNCTION ASIN(IND(B)).                      IF1034.2
            049800     IF (WS-NUM >= MIN-RANGE) AND                                 IF1034.2
            049900        (WS-NUM <= MAX-RANGE) THEN                                IF1034.2
            050000                    PERFORM PASS                                  IF1034.2
            050100     ELSE                                                         IF1034.2
            050200                    MOVE WS-NUM TO COMPUTED-N                     IF1034.2
            050300                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1034.2
            050400                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1034.2
            050500                    PERFORM FAIL.                                 IF1034.2
            050600     GO TO F-ASIN-WRITE-11.                                       IF1034.2
            050700 F-ASIN-DELETE-11.                                                IF1034.2
            050800     PERFORM  DE-LETE.                                            IF1034.2
            050900     GO TO    F-ASIN-WRITE-11.                                    IF1034.2
            051000 F-ASIN-WRITE-11.                                                 IF1034.2
            051100     MOVE "F-ASIN-11" TO PAR-NAME.                                IF1034.2
            051200     PERFORM  PRINT-DETAIL.                                       IF1034.2
            051300*****************TEST (a) - COMPLEX TEST****************          IF1034.2
            051400 F-ASIN-12.                                                       IF1034.2
            051500     MOVE ZERO TO WS-NUM.                                         IF1034.2
            051600     MOVE  0.785367 TO MIN-RANGE.                                 IF1034.2
            051700     MOVE  0.785429 TO MAX-RANGE.                                 IF1034.2
            051800 F-ASIN-TEST-12.                                                  IF1034.2
            051900     COMPUTE WS-NUM = FUNCTION ASIN(1 / SQRT2).                   IF1034.2
            052000     IF (WS-NUM >= MIN-RANGE) AND                                 IF1034.2
            052100        (WS-NUM <= MAX-RANGE) THEN                                IF1034.2
            052200                    PERFORM PASS                                  IF1034.2
            052300     ELSE                                                         IF1034.2
            052400                    MOVE WS-NUM TO COMPUTED-N                     IF1034.2
            052500                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1034.2
            052600                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1034.2
            052700                    PERFORM FAIL.                                 IF1034.2
            052800     GO TO F-ASIN-WRITE-12.                                       IF1034.2
            052900 F-ASIN-DELETE-12.                                                IF1034.2
            053000     PERFORM  DE-LETE.                                            IF1034.2
            053100     GO TO    F-ASIN-WRITE-12.                                    IF1034.2
            053200 F-ASIN-WRITE-12.                                                 IF1034.2
            053300     MOVE "F-ASIN-12" TO PAR-NAME.                                IF1034.2
            053400     PERFORM  PRINT-DETAIL.                                       IF1034.2
            053500*****************TEST (b) COMPLEX-TEST******************          IF1034.2
            053600 F-ASIN-13.                                                       IF1034.2
            053700     MOVE ZERO TO WS-NUM.                                         IF1034.2
            053800     MOVE  1.04715 TO MIN-RANGE.                                  IF1034.2
            053900     MOVE  1.04723 TO MAX-RANGE.                                  IF1034.2
            054000 F-ASIN-TEST-13.                                                  IF1034.2
            054100     COMPUTE WS-NUM = FUNCTION ASIN(SQRT3D2).                     IF1034.2
            054200     IF (WS-NUM >= MIN-RANGE) AND                                 IF1034.2
            054300        (WS-NUM <= MAX-RANGE) THEN                                IF1034.2
            054400                    PERFORM PASS                                  IF1034.2
            054500     ELSE                                                         IF1034.2
            054600                    MOVE WS-NUM TO COMPUTED-N                     IF1034.2
            054700                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1034.2
            054800                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1034.2
            054900                    PERFORM FAIL.                                 IF1034.2
            055000     GO TO F-ASIN-WRITE-13.                                       IF1034.2
            055100 F-ASIN-DELETE-13.                                                IF1034.2
            055200     PERFORM  DE-LETE.                                            IF1034.2
            055300     GO TO    F-ASIN-WRITE-13.                                    IF1034.2
            055400 F-ASIN-WRITE-13.                                                 IF1034.2
            055500     MOVE "F-ASIN-13" TO PAR-NAME.                                IF1034.2
            055600     PERFORM  PRINT-DETAIL.                                       IF1034.2
            055700*****************TEST (d) - COMPLEX TEST****************          IF1034.2
            055800 F-ASIN-15.                                                       IF1034.2
            055900     MOVE ZERO TO WS-NUM.                                         IF1034.2
            056000     MOVE  1.42919 TO MIN-RANGE.                                  IF1034.2
            056100     MOVE  1.42931 TO MAX-RANGE.                                  IF1034.2
            056200 F-ASIN-TEST-15.                                                  IF1034.2
            056300     COMPUTE WS-NUM = FUNCTION ASIN(1.98 / 2).                    IF1034.2
            056400     IF (WS-NUM >= MIN-RANGE) AND                                 IF1034.2
            056500        (WS-NUM <= MAX-RANGE) THEN                                IF1034.2
            056600                    PERFORM PASS                                  IF1034.2
            056700     ELSE                                                         IF1034.2
            056800                    MOVE WS-NUM TO COMPUTED-N                     IF1034.2
            056900                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1034.2
            057000                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1034.2
            057100                    PERFORM FAIL.                                 IF1034.2
            057200     GO TO F-ASIN-WRITE-15.                                       IF1034.2
            057300 F-ASIN-DELETE-15.                                                IF1034.2
            057400     PERFORM  DE-LETE.                                            IF1034.2
            057500     GO TO    F-ASIN-WRITE-15.                                    IF1034.2
            057600 F-ASIN-WRITE-15.                                                 IF1034.2
            057700     MOVE "F-ASIN-15" TO PAR-NAME.                                IF1034.2
            057800     PERFORM  PRINT-DETAIL.                                       IF1034.2
            057900*****************TEST (e) - COMPLEX TEST****************          IF1034.2
            058000 F-ASIN-16.                                                       IF1034.2
            058100     MOVE ZERO TO WS-NUM.                                         IF1034.2
            058200     MOVE  0.512069  TO MIN-RANGE.                                IF1034.2
            058300     MOVE  0.512110 TO MAX-RANGE.                                 IF1034.2
            058400 F-ASIN-TEST-16.                                                  IF1034.2
            058500     COMPUTE WS-NUM = FUNCTION ASIN(0.2 + 0.29).                  IF1034.2
            058600     IF (WS-NUM >= MIN-RANGE) AND                                 IF1034.2
            058700        (WS-NUM <= MAX-RANGE) THEN                                IF1034.2
            058800                    PERFORM PASS                                  IF1034.2
            058900     ELSE                                                         IF1034.2
            059000                    MOVE WS-NUM TO COMPUTED-N                     IF1034.2
            059100                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1034.2
            059200                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1034.2
            059300                    PERFORM FAIL.                                 IF1034.2
            059400     GO TO F-ASIN-WRITE-16.                                       IF1034.2
            059500 F-ASIN-DELETE-16.                                                IF1034.2
            059600     PERFORM  DE-LETE.                                            IF1034.2
            059700     GO TO    F-ASIN-WRITE-16.                                    IF1034.2
            059800 F-ASIN-WRITE-16.                                                 IF1034.2
            059900     MOVE "F-ASIN-16" TO PAR-NAME.                                IF1034.2
            060000     PERFORM  PRINT-DETAIL.                                       IF1034.2
            060100*****************TEST (f) - COMPLEX TEST****************          IF1034.2
            060200 F-ASIN-17.                                                       IF1034.2
            060300     MOVE ZERO TO WS-NUM.                                         IF1034.2
            060400     MOVE -1.42931 TO MIN-RANGE.                                  IF1034.2
            060500     MOVE -1.42919 TO MAX-RANGE.                                  IF1034.2
            060600 F-ASIN-TEST-17.                                                  IF1034.2
            060700     COMPUTE WS-NUM = FUNCTION ASIN(0.99 * -1).                   IF1034.2
            060800     IF (WS-NUM >= MIN-RANGE) AND                                 IF1034.2
            060900        (WS-NUM <= MAX-RANGE) THEN                                IF1034.2
            061000                    PERFORM PASS                                  IF1034.2
            061100     ELSE                                                         IF1034.2
            061200                    MOVE WS-NUM TO COMPUTED-N                     IF1034.2
            061300                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1034.2
            061400                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1034.2
            061500                    PERFORM FAIL.                                 IF1034.2
            061600     GO TO F-ASIN-WRITE-17.                                       IF1034.2
            061700 F-ASIN-DELETE-17.                                                IF1034.2
            061800     PERFORM  DE-LETE.                                            IF1034.2
            061900     GO TO    F-ASIN-WRITE-17.                                    IF1034.2
            062000 F-ASIN-WRITE-17.                                                 IF1034.2
            062100     MOVE "F-ASIN-17" TO PAR-NAME.                                IF1034.2
            062200     PERFORM  PRINT-DETAIL.                                       IF1034.2
            062300*****************TEST (g) - COMPLEX TEST****************          IF1034.2
            062400 F-ASIN-18.                                                       IF1034.2
            062500     MOVE ZERO TO WS-NUM.                                         IF1034.2
            062600     MOVE  0.675104  TO MIN-RANGE.                                IF1034.2
            062700     MOVE  0.675158 TO MAX-RANGE.                                 IF1034.2
            062800 F-ASIN-TEST-18.                                                  IF1034.2
            062900     COMPUTE WS-NUM = FUNCTION ASIN(IND(3) / 8).                  IF1034.2
            063000     IF (WS-NUM >= MIN-RANGE) AND                                 IF1034.2
            063100        (WS-NUM <= MAX-RANGE) THEN                                IF1034.2
            063200                    PERFORM PASS                                  IF1034.2
            063300     ELSE                                                         IF1034.2
            063400                    MOVE WS-NUM TO COMPUTED-N                     IF1034.2
            063500                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1034.2
            063600                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1034.2
            063700                    PERFORM FAIL.                                 IF1034.2
            063800     GO TO F-ASIN-WRITE-18.                                       IF1034.2
            063900 F-ASIN-DELETE-18.                                                IF1034.2
            064000     PERFORM  DE-LETE.                                            IF1034.2
            064100     GO TO    F-ASIN-WRITE-18.                                    IF1034.2
            064200 F-ASIN-WRITE-18.                                                 IF1034.2
            064300     MOVE "F-ASIN-18" TO PAR-NAME.                                IF1034.2
            064400     PERFORM  PRINT-DETAIL.                                       IF1034.2
            064500*****************TEST (h) - COMPLEX TEST****************          IF1034.2
            064600 F-ASIN-19.                                                       IF1034.2
            064700     MOVE ZERO TO WS-NUM.                                         IF1034.2
            064800     MOVE  1.57073 TO MIN-RANGE.                                  IF1034.2
            064900     MOVE  1.57080 TO MAX-RANGE.                                  IF1034.2
            065000 F-ASIN-TEST-19.                                                  IF1034.2
            065100     COMPUTE WS-NUM = FUNCTION ASIN(4 - 3).                       IF1034.2
            065200     IF (WS-NUM >= MIN-RANGE) AND                                 IF1034.2
            065300        (WS-NUM <= MAX-RANGE) THEN                                IF1034.2
            065400                    PERFORM PASS                                  IF1034.2
            065500     ELSE                                                         IF1034.2
            065600                    MOVE WS-NUM TO COMPUTED-N                     IF1034.2
            065700                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1034.2
            065800                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1034.2
            065900                    PERFORM FAIL.                                 IF1034.2
            066000     GO TO F-ASIN-WRITE-19.                                       IF1034.2
            066100 F-ASIN-DELETE-19.                                                IF1034.2
            066200     PERFORM  DE-LETE.                                            IF1034.2
            066300     GO TO    F-ASIN-WRITE-19.                                    IF1034.2
            066400 F-ASIN-WRITE-19.                                                 IF1034.2
            066500     MOVE "F-ASIN-19" TO PAR-NAME.                                IF1034.2
            066600     PERFORM  PRINT-DETAIL.                                       IF1034.2
            066700*****************TEST (i) - COMPLEX TEST****************          IF1034.2
            066800 F-ASIN-20.                                                       IF1034.2
            066900     MOVE ZERO TO WS-NUM.                                         IF1034.2
            067000     MOVE -0.000040 TO MIN-RANGE.                                 IF1034.2
            067100     MOVE  0.000040 TO MAX-RANGE.                                 IF1034.2
            067200 F-ASIN-TEST-20.                                                  IF1034.2
            067300     COMPUTE WS-NUM = FUNCTION ASIN(C - C).                       IF1034.2
            067400     IF (WS-NUM >= MIN-RANGE) AND                                 IF1034.2
            067500        (WS-NUM <= MAX-RANGE) THEN                                IF1034.2
            067600                    PERFORM PASS                                  IF1034.2
            067700     ELSE                                                         IF1034.2
            067800                    MOVE WS-NUM TO COMPUTED-N                     IF1034.2
            067900                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1034.2
            068000                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1034.2
            068100                    PERFORM FAIL.                                 IF1034.2
            068200     GO TO F-ASIN-WRITE-20.                                       IF1034.2
            068300 F-ASIN-DELETE-20.                                                IF1034.2
            068400     PERFORM  DE-LETE.                                            IF1034.2
            068500     GO TO    F-ASIN-WRITE-20.                                    IF1034.2
            068600 F-ASIN-WRITE-20.                                                 IF1034.2
            068700     MOVE "F-ASIN-20" TO PAR-NAME.                                IF1034.2
            068800     PERFORM  PRINT-DETAIL.                                       IF1034.2
            068900*****************TEST (j) - COMPLEX TEST****************          IF1034.2
            069000 F-ASIN-21.                                                       IF1034.2
            069100     MOVE ZERO TO WS-NUM.                                         IF1034.2
            069200     MOVE  0.252670 TO MIN-RANGE.                                 IF1034.2
            069300     MOVE  0.252690 TO MAX-RANGE.                                 IF1034.2
            069400 F-ASIN-TEST-21.                                                  IF1034.2
            069500     COMPUTE WS-NUM = FUNCTION ASIN(0.25 * 1).                    IF1034.2
            069600     IF (WS-NUM >= MIN-RANGE) AND                                 IF1034.2
            069700        (WS-NUM <= MAX-RANGE) THEN                                IF1034.2
            069800                    PERFORM PASS                                  IF1034.2
            069900     ELSE                                                         IF1034.2
            070000                    MOVE WS-NUM TO COMPUTED-N                     IF1034.2
            070100                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1034.2
            070200                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1034.2
            070300                    PERFORM FAIL.                                 IF1034.2
            070400     GO TO F-ASIN-WRITE-21.                                       IF1034.2
            070500 F-ASIN-DELETE-21.                                                IF1034.2
            070600     PERFORM  DE-LETE.                                            IF1034.2
            070700     GO TO    F-ASIN-WRITE-21.                                    IF1034.2
            070800 F-ASIN-WRITE-21.                                                 IF1034.2
            070900     MOVE "F-ASIN-21" TO PAR-NAME.                                IF1034.2
            071000     PERFORM  PRINT-DETAIL.                                       IF1034.2
            071100*****************TEST (k) - COMPLEX TEST****************          IF1034.2
            071200 F-ASIN-22.                                                       IF1034.2
            071300     MOVE ZERO TO WS-NUM.                                         IF1034.2
            071400     MOVE  0.323933 TO MIN-RANGE.                                 IF1034.2
            071500     MOVE  0.323959 TO MAX-RANGE.                                 IF1034.2
            071600 F-ASIN-TEST-22.                                                  IF1034.2
            071700     COMPUTE WS-NUM = FUNCTION ASIN(1 / PI).                      IF1034.2
            071800     IF (WS-NUM >= MIN-RANGE) AND                                 IF1034.2
            071900        (WS-NUM <= MAX-RANGE) THEN                                IF1034.2
            072000                    PERFORM PASS                                  IF1034.2
            072100     ELSE                                                         IF1034.2
            072200                    MOVE WS-NUM TO COMPUTED-N                     IF1034.2
            072300                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1034.2
            072400                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1034.2
            072500                    PERFORM FAIL.                                 IF1034.2
            072600     GO TO F-ASIN-WRITE-22.                                       IF1034.2
            072700 F-ASIN-DELETE-22.                                                IF1034.2
            072800     PERFORM  DE-LETE.                                            IF1034.2
            072900     GO TO    F-ASIN-WRITE-22.                                    IF1034.2
            073000 F-ASIN-WRITE-22.                                                 IF1034.2
            073100     MOVE "F-ASIN-22" TO PAR-NAME.                                IF1034.2
            073200     PERFORM  PRINT-DETAIL.                                       IF1034.2
            073300*****************TEST (l) - COMPLEX TEST****************          IF1034.2
            073400 F-ASIN-23.                                                       IF1034.2
            073500     MOVE ZERO TO WS-NUM.                                         IF1034.2
            073600     MOVE -0.000040 TO MIN-RANGE.                                 IF1034.2
            073700     MOVE  0.000040 TO MAX-RANGE.                                 IF1034.2
            073800 F-ASIN-TEST-23.                                                  IF1034.2
            073900     COMPUTE WS-NUM = FUNCTION ASIN((D / D) - 1).                 IF1034.2
            074000     IF (WS-NUM >= MIN-RANGE) AND                                 IF1034.2
            074100        (WS-NUM <= MAX-RANGE) THEN                                IF1034.2
            074200                    PERFORM PASS                                  IF1034.2
            074300     ELSE                                                         IF1034.2
            074400                    MOVE WS-NUM TO COMPUTED-N                     IF1034.2
            074500                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1034.2
            074600                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1034.2
            074700                    PERFORM FAIL.                                 IF1034.2
            074800     GO TO F-ASIN-WRITE-23.                                       IF1034.2
            074900 F-ASIN-DELETE-23.                                                IF1034.2
            075000     PERFORM  DE-LETE.                                            IF1034.2
            075100     GO TO    F-ASIN-WRITE-23.                                    IF1034.2
            075200 F-ASIN-WRITE-23.                                                 IF1034.2
            075300     MOVE "F-ASIN-23" TO PAR-NAME.                                IF1034.2
            075400     PERFORM  PRINT-DETAIL.                                       IF1034.2
            075500*****************TEST (m) - COMPLEX TEST****************          IF1034.2
            075600 F-ASIN-24.                                                       IF1034.2
            075700     MOVE ZERO TO WS-NUM.                                         IF1034.2
            075800     MOVE -1.03219 TO MIN-RANGE.                                  IF1034.2
            075900     MOVE -1.03211 TO MAX-RANGE.                                  IF1034.2
            076000 F-ASIN-TEST-24.                                                  IF1034.2
            076100     COMPUTE WS-NUM = FUNCTION ASIN(PI - 4).                      IF1034.2
            076200     IF (WS-NUM >= MIN-RANGE) AND                                 IF1034.2
            076300        (WS-NUM <= MAX-RANGE) THEN                                IF1034.2
            076400                    PERFORM PASS                                  IF1034.2
            076500     ELSE                                                         IF1034.2
            076600                    MOVE WS-NUM TO COMPUTED-N                     IF1034.2
            076700                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1034.2
            076800                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1034.2
            076900                    PERFORM FAIL.                                 IF1034.2
            077000     GO TO F-ASIN-WRITE-24.                                       IF1034.2
            077100 F-ASIN-DELETE-24.                                                IF1034.2
            077200     PERFORM  DE-LETE.                                            IF1034.2
            077300     GO TO    F-ASIN-WRITE-24.                                    IF1034.2
            077400 F-ASIN-WRITE-24.                                                 IF1034.2
            077500     MOVE "F-ASIN-24" TO PAR-NAME.                                IF1034.2
            077600     PERFORM  PRINT-DETAIL.                                       IF1034.2
            077700*****************TEST (n) - COMPLEX TEST****************          IF1034.2
            077800 F-ASIN-25.                                                       IF1034.2
            077900     MOVE ZERO TO WS-NUM.                                         IF1034.2
            078000     MOVE 0.142546 TO MIN-RANGE.                                  IF1034.2
            078100     MOVE 0.142558 TO MAX-RANGE.                                  IF1034.2
            078200 F-ASIN-TEST-25.                                                  IF1034.2
            078300     COMPUTE WS-NUM = FUNCTION ASIN(FUNCTION ASIN(PI - 3)).       IF1034.2
            078400     IF (WS-NUM >= MIN-RANGE) AND                                 IF1034.2
            078500        (WS-NUM <= MAX-RANGE) THEN                                IF1034.2
            078600                    PERFORM PASS                                  IF1034.2
            078700     ELSE                                                         IF1034.2
            078800                    MOVE WS-NUM TO COMPUTED-N                     IF1034.2
            078900                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1034.2
            079000                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1034.2
            079100                    PERFORM FAIL.                                 IF1034.2
            079200     GO TO F-ASIN-WRITE-25.                                       IF1034.2
            079300 F-ASIN-DELETE-25.                                                IF1034.2
            079400     PERFORM  DE-LETE.                                            IF1034.2
            079500     GO TO    F-ASIN-WRITE-25.                                    IF1034.2
            079600 F-ASIN-WRITE-25.                                                 IF1034.2
            079700     MOVE "F-ASIN-25" TO PAR-NAME.                                IF1034.2
            079800     PERFORM  PRINT-DETAIL.                                       IF1034.2
            079900*****************TEST (o) - COMPLEX TEST****************          IF1034.2
            080000 F-ASIN-26.                                                       IF1034.2
            080100     MOVE ZERO TO WS-NUM.                                         IF1034.2
            080200     MOVE 1.28695 TO MIN-RANGE.                                   IF1034.2
            080300     MOVE 1.28705 TO MAX-RANGE.                                   IF1034.2
            080400 F-ASIN-TEST-26.                                                  IF1034.2
            080500     COMPUTE WS-NUM = FUNCTION ASIN(0.6) +                        IF1034.2
            080600                      FUNCTION ASIN(0.6).                         IF1034.2
            080700     IF (WS-NUM >= MIN-RANGE) AND                                 IF1034.2
            080800        (WS-NUM <= MAX-RANGE) THEN                                IF1034.2
            080900                    PERFORM PASS                                  IF1034.2
            081000     ELSE                                                         IF1034.2
            081100                    MOVE WS-NUM TO COMPUTED-N                     IF1034.2
            081200                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1034.2
            081300                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1034.2
            081400                    PERFORM FAIL.                                 IF1034.2
            081500     GO TO F-ASIN-WRITE-26.                                       IF1034.2
            081600 F-ASIN-DELETE-26.                                                IF1034.2
            081700     PERFORM  DE-LETE.                                            IF1034.2
            081800     GO TO    F-ASIN-WRITE-26.                                    IF1034.2
            081900 F-ASIN-WRITE-26.                                                 IF1034.2
            082000     MOVE "F-ASIN-26" TO PAR-NAME.                                IF1034.2
            082100     PERFORM  PRINT-DETAIL.                                       IF1034.2
            082200*****************SPECIAL PERFORM TEST**********************       IF1034.2
            082300 F-ASIN-27.                                                       IF1034.2
            082400     MOVE ZERO TO WS-NUM.                                         IF1034.2
            082500     PERFORM F-ASIN-TEST-27                                       IF1034.2
            082600       UNTIL FUNCTION ASIN(ARG1) < 0.                             IF1034.2
            082700     PERFORM PASS.                                                IF1034.2
            082800     GO TO F-ASIN-WRITE-27.                                       IF1034.2
            082900 F-ASIN-TEST-27.                                                  IF1034.2
            083000     COMPUTE ARG1 = ARG1 - 0.25.                                  IF1034.2
            083100 F-ASIN-DELETE-27.                                                IF1034.2
            083200     PERFORM  DE-LETE.                                            IF1034.2
            083300     GO TO    F-ASIN-WRITE-27.                                    IF1034.2
            083400 F-ASIN-WRITE-27.                                                 IF1034.2
            083500     MOVE "F-ASIN-27" TO PAR-NAME.                                IF1034.2
            083600     PERFORM  PRINT-DETAIL.                                       IF1034.2
            083700********************END OF TESTS***************                   IF1034.2
            083800 CCVS-EXIT SECTION.                                               IF1034.2
            083900 CCVS-999999.                                                     IF1034.2
            084000     GO TO CLOSE-FILES.                                           IF1034.2
                  *END-OF,IF103A                                                                  
        """)
    )

    @Test
    fun if1044_2() = rewriteRun(
        cobol("""
                  *HEADER,COBOL,IF104A                                                            
            000100 IDENTIFICATION DIVISION.                                         IF1044.2
            000200 PROGRAM-ID.                                                      IF1044.2
            000300     IF104A.                                                      IF1044.2
            000400                                                                  IF1044.2
            000500***********************************************************       IF1044.2
            000600*                                                         *       IF1044.2
            000700*   This program is intended to form part of the CCVS85   *       IF1044.2
            000800*   COBOL Test Suite. It contains tests for the           *       IF1044.2
            000900*   Intrinsic Function ATAN.                              *       IF1044.2
            001000*                                                         *       IF1044.2
            001100***********************************************************       IF1044.2
            001200 ENVIRONMENT DIVISION.                                            IF1044.2
            001300 CONFIGURATION SECTION.                                           IF1044.2
            001400 SOURCE-COMPUTER.                                                 IF1044.2
            001500     XXXXX082.                                                    IF1044.2
            001600 OBJECT-COMPUTER.                                                 IF1044.2
            001700     XXXXX083.                                                    IF1044.2
            001800 INPUT-OUTPUT SECTION.                                            IF1044.2
            001900 FILE-CONTROL.                                                    IF1044.2
            002000     SELECT PRINT-FILE ASSIGN TO                                  IF1044.2
            002100     XXXXX055.                                                    IF1044.2
            002200 DATA DIVISION.                                                   IF1044.2
            002300 FILE SECTION.                                                    IF1044.2
            002400 FD  PRINT-FILE.                                                  IF1044.2
            002500 01  PRINT-REC PICTURE X(120).                                    IF1044.2
            002600 01  DUMMY-RECORD PICTURE X(120).                                 IF1044.2
            002700 WORKING-STORAGE SECTION.                                         IF1044.2
            002800***********************************************************       IF1044.2
            002900* Variables specific to the Intrinsic Function Test IF104A*       IF1044.2
            003000***********************************************************       IF1044.2
            003100 01  A                   PIC S9(5)V9(5)      VALUE -0.00004.      IF1044.2
            003200 01  B                   PIC S9(10)          VALUE 2.             IF1044.2
            003300 01  C                   PIC S9(10)          VALUE 100000.        IF1044.2
            003400 01  D                   PIC S9(10)          VALUE 1000.          IF1044.2
            003500 01  PI                  PIC S9V9(17)        VALUE 3.141592654.   IF1044.2
            003600 01  ARG1                PIC S9V9(17)        VALUE 1.00.          IF1044.2
            003700 01  SQRT3               PIC S9V9(17)        VALUE 1.732050808.   IF1044.2
            003800 01  ARR                                     VALUE "40537".       IF1044.2
            003900     02  IND OCCURS 5 TIMES PIC 9.                                IF1044.2
            004000 01  TEMP                PIC S9(5)V9(5).                          IF1044.2
            004100 01  WS-NUM              PIC S9(5)V9(6).                          IF1044.2
            004200 01  MIN-RANGE           PIC S9(5)V9(7).                          IF1044.2
            004300 01  MAX-RANGE           PIC S9(5)V9(7).                          IF1044.2
            004400*                                                                 IF1044.2
            004500**********************************************************        IF1044.2
            004600*                                                                 IF1044.2
            004700 01  TEST-RESULTS.                                                IF1044.2
            004800     02 FILLER                   PIC X      VALUE SPACE.          IF1044.2
            004900     02 FEATURE                  PIC X(20)  VALUE SPACE.          IF1044.2
            005000     02 FILLER                   PIC X      VALUE SPACE.          IF1044.2
            005100     02 P-OR-F                   PIC X(5)   VALUE SPACE.          IF1044.2
            005200     02 FILLER                   PIC X      VALUE SPACE.          IF1044.2
            005300     02  PAR-NAME.                                                IF1044.2
            005400       03 FILLER                 PIC X(19)  VALUE SPACE.          IF1044.2
            005500       03  PARDOT-X              PIC X      VALUE SPACE.          IF1044.2
            005600       03 DOTVALUE               PIC 99     VALUE ZERO.           IF1044.2
            005700     02 FILLER                   PIC X(8)   VALUE SPACE.          IF1044.2
            005800     02 RE-MARK                  PIC X(61).                       IF1044.2
            005900 01  TEST-COMPUTED.                                               IF1044.2
            006000     02 FILLER                   PIC X(30)  VALUE SPACE.          IF1044.2
            006100     02 FILLER                   PIC X(17)  VALUE                 IF1044.2
            006200            "       COMPUTED=".                                   IF1044.2
            006300     02 COMPUTED-X.                                               IF1044.2
            006400     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          IF1044.2
            006500     03 COMPUTED-N               REDEFINES COMPUTED-A             IF1044.2
            006600                                 PIC -9(9).9(9).                  IF1044.2
            006700     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         IF1044.2
            006800     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     IF1044.2
            006900     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     IF1044.2
            007000     03       CM-18V0 REDEFINES COMPUTED-A.                       IF1044.2
            007100         04 COMPUTED-18V0                    PIC -9(18).          IF1044.2
            007200         04 FILLER                           PIC X.               IF1044.2
            007300     03 FILLER PIC X(50) VALUE SPACE.                             IF1044.2
            007400 01  TEST-CORRECT.                                                IF1044.2
            007500     02 FILLER PIC X(30) VALUE SPACE.                             IF1044.2
            007600     02 FILLER PIC X(17) VALUE "       CORRECT =".                IF1044.2
            007700     02 CORRECT-X.                                                IF1044.2
            007800     03 CORRECT-A                  PIC X(20) VALUE SPACE.         IF1044.2
            007900     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      IF1044.2
            008000     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         IF1044.2
            008100     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     IF1044.2
            008200     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     IF1044.2
            008300     03      CR-18V0 REDEFINES CORRECT-A.                         IF1044.2
            008400         04 CORRECT-18V0                     PIC -9(18).          IF1044.2
            008500         04 FILLER                           PIC X.               IF1044.2
            008600     03 FILLER PIC X(2) VALUE SPACE.                              IF1044.2
            008700     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     IF1044.2
            008800 01  TEST-CORRECT-MIN.                                            IF1044.2
            008900     02 FILLER PIC X(30) VALUE SPACE.                             IF1044.2
            009000     02 FILLER PIC X(17) VALUE "     MIN VALUE =".                IF1044.2
            009100     02 CORRECTMI-X.                                              IF1044.2
            009200     03 CORRECTMI-A                 PIC X(20) VALUE SPACE.        IF1044.2
            009300     03 CORRECT-MIN    REDEFINES CORRECTMI-A     PIC -9(9).9(9).  IF1044.2
            009400     03 CORRECTMI-0V18 REDEFINES CORRECTMI-A     PIC -.9(18).     IF1044.2
            009500     03 CORRECTMI-4V14 REDEFINES CORRECTMI-A     PIC -9(4).9(14). IF1044.2
            009600     03 CORRECTMI-14V4 REDEFINES CORRECTMI-A     PIC -9(14).9(4). IF1044.2
            009700     03      CR-18V0 REDEFINES CORRECTMI-A.                       IF1044.2
            009800         04 CORRECTMI-18V0                     PIC -9(18).        IF1044.2
            009900         04 FILLER                           PIC X.               IF1044.2
            010000     03 FILLER PIC X(2) VALUE SPACE.                              IF1044.2
            010100     03 FILLER                           PIC X(48) VALUE SPACE.   IF1044.2
            010200 01  TEST-CORRECT-MAX.                                            IF1044.2
            010300     02 FILLER PIC X(30) VALUE SPACE.                             IF1044.2
            010400     02 FILLER PIC X(17) VALUE "     MAX VALUE =".                IF1044.2
            010500     02 CORRECTMA-X.                                              IF1044.2
            010600     03 CORRECTMA-A                  PIC X(20) VALUE SPACE.       IF1044.2
            010700     03 CORRECT-MAX    REDEFINES CORRECTMA-A     PIC -9(9).9(9).  IF1044.2
            010800     03 CORRECTMA-0V18 REDEFINES CORRECTMA-A     PIC -.9(18).     IF1044.2
            010900     03 CORRECTMA-4V14 REDEFINES CORRECTMA-A     PIC -9(4).9(14). IF1044.2
            011000     03 CORRECTMA-14V4 REDEFINES CORRECTMA-A     PIC -9(14).9(4). IF1044.2
            011100     03      CR-18V0 REDEFINES CORRECTMA-A.                       IF1044.2
            011200         04 CORRECTMA-18V0                     PIC -9(18).        IF1044.2
            011300         04 FILLER                           PIC X.               IF1044.2
            011400     03 FILLER PIC X(2) VALUE SPACE.                              IF1044.2
            011500     03 CORMA-ANSI-REFERENCE             PIC X(48) VALUE SPACE.   IF1044.2
            011600 01  CCVS-C-1.                                                    IF1044.2
            011700     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PAIF1044.2
            011800-    "SS  PARAGRAPH-NAME                                          IF1044.2
            011900-    "       REMARKS".                                            IF1044.2
            012000     02 FILLER                     PIC X(20)    VALUE SPACE.      IF1044.2
            012100 01  CCVS-C-2.                                                    IF1044.2
            012200     02 FILLER                     PIC X        VALUE SPACE.      IF1044.2
            012300     02 FILLER                     PIC X(6)     VALUE "TESTED".   IF1044.2
            012400     02 FILLER                     PIC X(15)    VALUE SPACE.      IF1044.2
            012500     02 FILLER                     PIC X(4)     VALUE "FAIL".     IF1044.2
            012600     02 FILLER                     PIC X(94)    VALUE SPACE.      IF1044.2
            012700 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       IF1044.2
            012800 01  REC-CT                        PIC 99       VALUE ZERO.       IF1044.2
            012900 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       IF1044.2
            013000 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       IF1044.2
            013100 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       IF1044.2
            013200 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       IF1044.2
            013300 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       IF1044.2
            013400 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       IF1044.2
            013500 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      IF1044.2
            013600 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       IF1044.2
            013700 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     IF1044.2
            013800 01  CCVS-H-1.                                                    IF1044.2
            013900     02  FILLER                    PIC X(39)    VALUE SPACES.     IF1044.2
            014000     02  FILLER                    PIC X(42)    VALUE             IF1044.2
            014100     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 IF1044.2
            014200     02  FILLER                    PIC X(39)    VALUE SPACES.     IF1044.2
            014300 01  CCVS-H-2A.                                                   IF1044.2
            014400   02  FILLER                        PIC X(40)  VALUE SPACE.      IF1044.2
            014500   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  IF1044.2
            014600   02  FILLER                        PIC XXXX   VALUE             IF1044.2
            014700     "4.2 ".                                                      IF1044.2
            014800   02  FILLER                        PIC X(28)  VALUE             IF1044.2
            014900            " COPY - NOT FOR DISTRIBUTION".                       IF1044.2
            015000   02  FILLER                        PIC X(41)  VALUE SPACE.      IF1044.2
            015100                                                                  IF1044.2
            015200 01  CCVS-H-2B.                                                   IF1044.2
            015300   02  FILLER                        PIC X(15)  VALUE             IF1044.2
            015400            "TEST RESULT OF ".                                    IF1044.2
            015500   02  TEST-ID                       PIC X(9).                    IF1044.2
            015600   02  FILLER                        PIC X(4)   VALUE             IF1044.2
            015700            " IN ".                                               IF1044.2
            015800   02  FILLER                        PIC X(12)  VALUE             IF1044.2
            015900     " HIGH       ".                                              IF1044.2
            016000   02  FILLER                        PIC X(22)  VALUE             IF1044.2
            016100            " LEVEL VALIDATION FOR ".                             IF1044.2
            016200   02  FILLER                        PIC X(58)  VALUE             IF1044.2
            016300     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".IF1044.2
            016400 01  CCVS-H-3.                                                    IF1044.2
            016500     02  FILLER                      PIC X(34)  VALUE             IF1044.2
            016600            " FOR OFFICIAL USE ONLY    ".                         IF1044.2
            016700     02  FILLER                      PIC X(58)  VALUE             IF1044.2
            016800     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".IF1044.2
            016900     02  FILLER                      PIC X(28)  VALUE             IF1044.2
            017000            "  COPYRIGHT   1985 ".                                IF1044.2
            017100 01  CCVS-E-1.                                                    IF1044.2
            017200     02 FILLER                       PIC X(52)  VALUE SPACE.      IF1044.2
            017300     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              IF1044.2
            017400     02 ID-AGAIN                     PIC X(9).                    IF1044.2
            017500     02 FILLER                       PIC X(45)  VALUE SPACES.     IF1044.2
            017600 01  CCVS-E-2.                                                    IF1044.2
            017700     02  FILLER                      PIC X(31)  VALUE SPACE.      IF1044.2
            017800     02  FILLER                      PIC X(21)  VALUE SPACE.      IF1044.2
            017900     02 CCVS-E-2-2.                                               IF1044.2
            018000         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      IF1044.2
            018100         03 FILLER                   PIC X      VALUE SPACE.      IF1044.2
            018200         03 ENDER-DESC               PIC X(44)  VALUE             IF1044.2
            018300            "ERRORS ENCOUNTERED".                                 IF1044.2
            018400 01  CCVS-E-3.                                                    IF1044.2
            018500     02  FILLER                      PIC X(22)  VALUE             IF1044.2
            018600            " FOR OFFICIAL USE ONLY".                             IF1044.2
            018700     02  FILLER                      PIC X(12)  VALUE SPACE.      IF1044.2
            018800     02  FILLER                      PIC X(58)  VALUE             IF1044.2
            018900     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".IF1044.2
            019000     02  FILLER                      PIC X(13)  VALUE SPACE.      IF1044.2
            019100     02 FILLER                       PIC X(15)  VALUE             IF1044.2
            019200             " COPYRIGHT 1985".                                   IF1044.2
            019300 01  CCVS-E-4.                                                    IF1044.2
            019400     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      IF1044.2
            019500     02 FILLER                       PIC X(4)   VALUE " OF ".     IF1044.2
            019600     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      IF1044.2
            019700     02 FILLER                       PIC X(40)  VALUE             IF1044.2
            019800      "  TESTS WERE EXECUTED SUCCESSFULLY".                       IF1044.2
            019900 01  XXINFO.                                                      IF1044.2
            020000     02 FILLER                       PIC X(19)  VALUE             IF1044.2
            020100            "*** INFORMATION ***".                                IF1044.2
            020200     02 INFO-TEXT.                                                IF1044.2
            020300       04 FILLER                     PIC X(8)   VALUE SPACE.      IF1044.2
            020400       04 XXCOMPUTED                 PIC X(20).                   IF1044.2
            020500       04 FILLER                     PIC X(5)   VALUE SPACE.      IF1044.2
            020600       04 XXCORRECT                  PIC X(20).                   IF1044.2
            020700     02 INF-ANSI-REFERENCE           PIC X(48).                   IF1044.2
            020800 01  HYPHEN-LINE.                                                 IF1044.2
            020900     02 FILLER  PIC IS X VALUE IS SPACE.                          IF1044.2
            021000     02 FILLER  PIC IS X(65)    VALUE IS "************************IF1044.2
            021100-    "*****************************************".                 IF1044.2
            021200     02 FILLER  PIC IS X(54)    VALUE IS "************************IF1044.2
            021300-    "******************************".                            IF1044.2
            021400 01  CCVS-PGM-ID                     PIC X(9)   VALUE             IF1044.2
            021500     "IF104A".                                                    IF1044.2
            021600 PROCEDURE DIVISION.                                              IF1044.2
            021700 CCVS1 SECTION.                                                   IF1044.2
            021800 OPEN-FILES.                                                      IF1044.2
            021900     OPEN     OUTPUT PRINT-FILE.                                  IF1044.2
            022000     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   IF1044.2
            022100     MOVE    SPACE TO TEST-RESULTS.                               IF1044.2
            022200     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             IF1044.2
            022300     GO TO CCVS1-EXIT.                                            IF1044.2
            022400 CLOSE-FILES.                                                     IF1044.2
            022500     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   IF1044.2
            022600 TERMINATE-CCVS.                                                  IF1044.2
            022700     STOP     RUN.                                                IF1044.2
            022800 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         IF1044.2
            022900 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           IF1044.2
            023000 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          IF1044.2
            023100 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      IF1044.2
            023200     MOVE "****TEST DELETED****" TO RE-MARK.                      IF1044.2
            023300 PRINT-DETAIL.                                                    IF1044.2
            023400     IF REC-CT NOT EQUAL TO ZERO                                  IF1044.2
            023500             MOVE "." TO PARDOT-X                                 IF1044.2
            023600             MOVE REC-CT TO DOTVALUE.                             IF1044.2
            023700     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      IF1044.2
            023800     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               IF1044.2
            023900        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 IF1044.2
            024000          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 IF1044.2
            024100     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              IF1044.2
            024200     MOVE SPACE TO CORRECT-X.                                     IF1044.2
            024300     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         IF1044.2
            024400     MOVE     SPACE TO RE-MARK.                                   IF1044.2
            024500 HEAD-ROUTINE.                                                    IF1044.2
            024600     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  IF1044.2
            024700     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  IF1044.2
            024800     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  IF1044.2
            024900     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  IF1044.2
            025000 COLUMN-NAMES-ROUTINE.                                            IF1044.2
            025100     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           IF1044.2
            025200     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1044.2
            025300     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        IF1044.2
            025400 END-ROUTINE.                                                     IF1044.2
            025500     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.IF1044.2
            025600 END-RTN-EXIT.                                                    IF1044.2
            025700     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1044.2
            025800 END-ROUTINE-1.                                                   IF1044.2
            025900      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      IF1044.2
            026000      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               IF1044.2
            026100      ADD PASS-COUNTER TO ERROR-HOLD.                             IF1044.2
            026200      MOVE PASS-COUNTER TO CCVS-E-4-1.                            IF1044.2
            026300      MOVE ERROR-HOLD TO CCVS-E-4-2.                              IF1044.2
            026400      MOVE CCVS-E-4 TO CCVS-E-2-2.                                IF1044.2
            026500      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           IF1044.2
            026600  END-ROUTINE-12.                                                 IF1044.2
            026700      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        IF1044.2
            026800     IF       ERROR-COUNTER IS EQUAL TO ZERO                      IF1044.2
            026900         MOVE "NO " TO ERROR-TOTAL                                IF1044.2
            027000         ELSE                                                     IF1044.2
            027100         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       IF1044.2
            027200     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           IF1044.2
            027300     PERFORM WRITE-LINE.                                          IF1044.2
            027400 END-ROUTINE-13.                                                  IF1044.2
            027500     IF DELETE-COUNTER IS EQUAL TO ZERO                           IF1044.2
            027600         MOVE "NO " TO ERROR-TOTAL  ELSE                          IF1044.2
            027700         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      IF1044.2
            027800     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   IF1044.2
            027900     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           IF1044.2
            028000      IF   INSPECT-COUNTER EQUAL TO ZERO                          IF1044.2
            028100          MOVE "NO " TO ERROR-TOTAL                               IF1044.2
            028200      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   IF1044.2
            028300      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            IF1044.2
            028400      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          IF1044.2
            028500     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           IF1044.2
            028600 WRITE-LINE.                                                      IF1044.2
            028700     ADD 1 TO RECORD-COUNT.                                       IF1044.2
            028800*    IF RECORD-COUNT GREATER 42                                   IF1044.2
            028900*        MOVE DUMMY-RECORD TO DUMMY-HOLD                          IF1044.2
            029000*        MOVE SPACE TO DUMMY-RECORD                               IF1044.2
            029100*        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  IF1044.2
            029200*        MOVE CCVS-H-1  TO DUMMY-RECORD  PERFORM WRT-LN 2 TIMES   IF1044.2
            029300*        MOVE CCVS-H-2A TO DUMMY-RECORD  PERFORM WRT-LN 2 TIMES   IF1044.2
            029400*        MOVE CCVS-H-2B TO DUMMY-RECORD  PERFORM WRT-LN 3 TIMES   IF1044.2
            029500*        MOVE CCVS-H-3  TO DUMMY-RECORD  PERFORM WRT-LN 3 TIMES   IF1044.2
            029600*        MOVE CCVS-C-1  TO DUMMY-RECORD  PERFORM WRT-LN           IF1044.2
            029700*        MOVE CCVS-C-2  TO DUMMY-RECORD  PERFORM WRT-LN           IF1044.2
            029800*        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          IF1044.2
            029900*        MOVE DUMMY-HOLD TO DUMMY-RECORD                          IF1044.2
            030000*        MOVE ZERO TO RECORD-COUNT.                               IF1044.2
            030100     PERFORM WRT-LN.                                              IF1044.2
            030200 WRT-LN.                                                          IF1044.2
            030300     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               IF1044.2
            030400     MOVE SPACE TO DUMMY-RECORD.                                  IF1044.2
            030500 BLANK-LINE-PRINT.                                                IF1044.2
            030600     PERFORM WRT-LN.                                              IF1044.2
            030700 FAIL-ROUTINE.                                                    IF1044.2
            030800     IF     COMPUTED-X NOT EQUAL TO SPACE                         IF1044.2
            030900            GO TO FAIL-ROUTINE-WRITE.                             IF1044.2
            031000     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.IF1044.2
            031100     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 IF1044.2
            031200     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   IF1044.2
            031300     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1044.2
            031400     MOVE   SPACES TO INF-ANSI-REFERENCE.                         IF1044.2
            031500     GO TO  FAIL-ROUTINE-EX.                                      IF1044.2
            031600 FAIL-ROUTINE-WRITE.                                              IF1044.2
            031700     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE.        IF1044.2
            031800     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE                  IF1044.2
            031900                              CORMA-ANSI-REFERENCE.               IF1044.2
            032000     IF CORRECT-MIN NOT EQUAL TO SPACES THEN                      IF1044.2
            032100           MOVE TEST-CORRECT-MIN TO PRINT-REC PERFORM WRITE-LINE  IF1044.2
            032200           MOVE TEST-CORRECT-MAX TO PRINT-REC PERFORM WRITE-LINE  IF1044.2
            032300     ELSE                                                         IF1044.2
            032400           MOVE TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE.     IF1044.2
            032500     PERFORM WRITE-LINE.                                          IF1044.2
            032600     MOVE   SPACES TO COR-ANSI-REFERENCE.                         IF1044.2
            032700 FAIL-ROUTINE-EX. EXIT.                                           IF1044.2
            032800 BAIL-OUT.                                                        IF1044.2
            032900     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   IF1044.2
            033000     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           IF1044.2
            033100 BAIL-OUT-WRITE.                                                  IF1044.2
            033200     MOVE CORRECT-A TO XXCORRECT.                                 IF1044.2
            033300     MOVE COMPUTED-A TO XXCOMPUTED.                               IF1044.2
            033400     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 IF1044.2
            033500     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1044.2
            033600     MOVE   SPACES TO INF-ANSI-REFERENCE.                         IF1044.2
            033700 BAIL-OUT-EX. EXIT.                                               IF1044.2
            033800 CCVS1-EXIT.                                                      IF1044.2
            033900     EXIT.                                                        IF1044.2
            034000********************************************************          IF1044.2
            034100*                                                      *          IF1044.2
            034200*    Intrinsic Function Tests         IF104A - ATAN    *          IF1044.2
            034300*                                                      *          IF1044.2
            034400********************************************************          IF1044.2
            034500 SECT-IF104A SECTION.                                             IF1044.2
            034600 F-ATAN-INFO.                                                     IF1044.2
            034700     MOVE     "See ref. A-36 2.8" TO ANSI-REFERENCE.              IF1044.2
            034800     MOVE     "ATAN Function" TO FEATURE.                         IF1044.2
            034900*****************TEST (a) - SIMPLE TEST*****************          IF1044.2
            035000 F-ATAN-01.                                                       IF1044.2
            035100     MOVE ZERO TO WS-NUM.                                         IF1044.2
            035200     MOVE  0.785382 TO MIN-RANGE.                                 IF1044.2
            035300     MOVE  0.785414 TO MAX-RANGE.                                 IF1044.2
            035400 F-ATAN-TEST-01.                                                  IF1044.2
            035500     COMPUTE WS-NUM = FUNCTION ATAN(1.0).                         IF1044.2
            035600     IF (WS-NUM >= MIN-RANGE) AND                                 IF1044.2
            035700        (WS-NUM <= MAX-RANGE) THEN                                IF1044.2
            035800                    PERFORM PASS                                  IF1044.2
            035900     ELSE                                                         IF1044.2
            036000                    MOVE WS-NUM TO COMPUTED-N                     IF1044.2
            036100                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1044.2
            036200                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1044.2
            036300                    PERFORM FAIL.                                 IF1044.2
            036400     GO TO F-ATAN-WRITE-01.                                       IF1044.2
            036500 F-ATAN-DELETE-01.                                                IF1044.2
            036600     PERFORM  DE-LETE.                                            IF1044.2
            036700     GO TO    F-ATAN-WRITE-01.                                    IF1044.2
            036800 F-ATAN-WRITE-01.                                                 IF1044.2
            036900     MOVE "F-ATAN-01" TO PAR-NAME.                                IF1044.2
            037000     PERFORM  PRINT-DETAIL.                                       IF1044.2
            037100*****************TEST (b) - SIMPLE TEST*****************          IF1044.2
            037200 F-ATAN-02.                                                       IF1044.2
            037300     EVALUATE FUNCTION ATAN(0.5)                                  IF1044.2
            037400     WHEN  0.463638 THRU 0.463656                                 IF1044.2
            037500                    PERFORM PASS                                  IF1044.2
            037600     WHEN OTHER                                                   IF1044.2
            037700                    PERFORM FAIL.                                 IF1044.2
            037800     GO TO F-ATAN-WRITE-02.                                       IF1044.2
            037900 F-ATAN-DELETE-02.                                                IF1044.2
            038000     PERFORM  DE-LETE.                                            IF1044.2
            038100     GO TO    F-ATAN-WRITE-02.                                    IF1044.2
            038200 F-ATAN-WRITE-02.                                                 IF1044.2
            038300     MOVE "F-ATAN-02" TO PAR-NAME.                                IF1044.2
            038400     PERFORM  PRINT-DETAIL.                                       IF1044.2
            038500*****************TEST (c) - SIMPLE TEST*****************          IF1044.2
            038600 F-ATAN-03.                                                       IF1044.2
            038700     MOVE -0.000020 TO MIN-RANGE.                                 IF1044.2
            038800     MOVE  0.000020 TO MAX-RANGE.                                 IF1044.2
            038900 F-ATAN-TEST-03.                                                  IF1044.2
            039000     IF (FUNCTION ATAN(0) >= MIN-RANGE) AND                       IF1044.2
            039100        (FUNCTION ATAN(0) <= MAX-RANGE) THEN                      IF1044.2
            039200                    PERFORM PASS                                  IF1044.2
            039300     ELSE                                                         IF1044.2
            039400                    PERFORM FAIL.                                 IF1044.2
            039500     GO TO F-ATAN-WRITE-03.                                       IF1044.2
            039600 F-ATAN-DELETE-03.                                                IF1044.2
            039700     PERFORM  DE-LETE.                                            IF1044.2
            039800     GO TO    F-ATAN-WRITE-03.                                    IF1044.2
            039900 F-ATAN-WRITE-03.                                                 IF1044.2
            040000     MOVE "F-ATAN-03" TO PAR-NAME.                                IF1044.2
            040100     PERFORM  PRINT-DETAIL.                                       IF1044.2
            040200*****************TEST (d) - SIMPLE TEST*****************          IF1044.2
            040300 F-ATAN-04.                                                       IF1044.2
            040400     MOVE ZERO TO WS-NUM.                                         IF1044.2
            040500     MOVE -0.785414  TO MIN-RANGE.                                IF1044.2
            040600     MOVE -0.785382 TO MAX-RANGE.                                 IF1044.2
            040700 F-ATAN-TEST-04.                                                  IF1044.2
            040800     COMPUTE WS-NUM = FUNCTION ATAN(-1).                          IF1044.2
            040900     IF (WS-NUM >= MIN-RANGE) AND                                 IF1044.2
            041000        (WS-NUM <= MAX-RANGE) THEN                                IF1044.2
            041100                    PERFORM PASS                                  IF1044.2
            041200     ELSE                                                         IF1044.2
            041300                    MOVE WS-NUM TO COMPUTED-N                     IF1044.2
            041400                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1044.2
            041500                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1044.2
            041600                    PERFORM FAIL.                                 IF1044.2
            041700     GO TO F-ATAN-WRITE-04.                                       IF1044.2
            041800 F-ATAN-DELETE-04.                                                IF1044.2
            041900     PERFORM  DE-LETE.                                            IF1044.2
            042000     GO TO    F-ATAN-WRITE-04.                                    IF1044.2
            042100 F-ATAN-WRITE-04.                                                 IF1044.2
            042200     MOVE "F-ATAN-04" TO PAR-NAME.                                IF1044.2
            042300     PERFORM  PRINT-DETAIL.                                       IF1044.2
            042400*****************TEST (e) - SIMPLE TEST*****************          IF1044.2
            042500 F-ATAN-05.                                                       IF1044.2
            042600     MOVE ZERO TO WS-NUM.                                         IF1044.2
            042700     MOVE  0.784881 TO MIN-RANGE.                                 IF1044.2
            042800     MOVE  0.784913 TO MAX-RANGE.                                 IF1044.2
            042900 F-ATAN-TEST-05.                                                  IF1044.2
            043000     COMPUTE WS-NUM = FUNCTION ATAN(.999).                        IF1044.2
            043100     IF (WS-NUM >= MIN-RANGE) AND                                 IF1044.2
            043200        (WS-NUM <= MAX-RANGE) THEN                                IF1044.2
            043300                    PERFORM PASS                                  IF1044.2
            043400     ELSE                                                         IF1044.2
            043500                    MOVE WS-NUM TO COMPUTED-N                     IF1044.2
            043600                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1044.2
            043700                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1044.2
            043800                    PERFORM FAIL.                                 IF1044.2
            043900     GO TO F-ATAN-WRITE-05.                                       IF1044.2
            044000 F-ATAN-DELETE-05.                                                IF1044.2
            044100     PERFORM  DE-LETE.                                            IF1044.2
            044200     GO TO    F-ATAN-WRITE-05.                                    IF1044.2
            044300 F-ATAN-WRITE-05.                                                 IF1044.2
            044400     MOVE "F-ATAN-05" TO PAR-NAME.                                IF1044.2
            044500     PERFORM  PRINT-DETAIL.                                       IF1044.2
            044600*****************TEST (f) - SIMPLE TEST*****************          IF1044.2
            044700 F-ATAN-06.                                                       IF1044.2
            044800     MOVE ZERO TO WS-NUM.                                         IF1044.2
            044900     MOVE  0.048959  TO MIN-RANGE.                                IF1044.2
            045000     MOVE  0.048961 TO MAX-RANGE.                                 IF1044.2
            045100 F-ATAN-TEST-06.                                                  IF1044.2
            045200     COMPUTE WS-NUM = FUNCTION ATAN(.049).                        IF1044.2
            045300     IF (WS-NUM >= MIN-RANGE) AND                                 IF1044.2
            045400        (WS-NUM <= MAX-RANGE) THEN                                IF1044.2
            045500                    PERFORM PASS                                  IF1044.2
            045600     ELSE                                                         IF1044.2
            045700                    MOVE WS-NUM TO COMPUTED-N                     IF1044.2
            045800                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1044.2
            045900                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1044.2
            046000                    PERFORM FAIL.                                 IF1044.2
            046100     GO TO F-ATAN-WRITE-06.                                       IF1044.2
            046200 F-ATAN-DELETE-06.                                                IF1044.2
            046300     PERFORM  DE-LETE.                                            IF1044.2
            046400     GO TO    F-ATAN-WRITE-06.                                    IF1044.2
            046500 F-ATAN-WRITE-06.                                                 IF1044.2
            046600     MOVE "F-ATAN-06" TO PAR-NAME.                                IF1044.2
            046700     PERFORM  PRINT-DETAIL.                                       IF1044.2
            046800*****************TEST (g) - SIMPLE TEST*****************          IF1044.2
            046900 F-ATAN-07.                                                       IF1044.2
            047000     MOVE ZERO TO WS-NUM.                                         IF1044.2
            047100     MOVE -0.000040 TO MIN-RANGE.                                 IF1044.2
            047200     MOVE -0.000039 TO MAX-RANGE.                                 IF1044.2
            047300 F-ATAN-TEST-07.                                                  IF1044.2
            047400     COMPUTE WS-NUM = FUNCTION ATAN(A).                           IF1044.2
            047500     IF (WS-NUM >= MIN-RANGE) AND                                 IF1044.2
            047600        (WS-NUM <= MAX-RANGE) THEN                                IF1044.2
            047700                    PERFORM PASS                                  IF1044.2
            047800     ELSE                                                         IF1044.2
            047900                    MOVE WS-NUM TO COMPUTED-N                     IF1044.2
            048000                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1044.2
            048100                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1044.2
            048200                    PERFORM FAIL.                                 IF1044.2
            048300     GO TO F-ATAN-WRITE-07.                                       IF1044.2
            048400 F-ATAN-DELETE-07.                                                IF1044.2
            048500     PERFORM  DE-LETE.                                            IF1044.2
            048600     GO TO    F-ATAN-WRITE-07.                                    IF1044.2
            048700 F-ATAN-WRITE-07.                                                 IF1044.2
            048800     MOVE "F-ATAN-07" TO PAR-NAME.                                IF1044.2
            048900     PERFORM  PRINT-DETAIL.                                       IF1044.2
            049000*****************TEST (h) - SIMPLE TEST*****************          IF1044.2
            049100 F-ATAN-08.                                                       IF1044.2
            049200     MOVE ZERO TO WS-NUM.                                         IF1044.2
            049300     MOVE  0.000019  TO MIN-RANGE.                                IF1044.2
            049400     MOVE  0.000020 TO MAX-RANGE.                                 IF1044.2
            049500 F-ATAN-TEST-08.                                                  IF1044.2
            049600     COMPUTE WS-NUM = FUNCTION ATAN(.00002).                      IF1044.2
            049700     IF (WS-NUM >= MIN-RANGE) AND                                 IF1044.2
            049800        (WS-NUM <= MAX-RANGE) THEN                                IF1044.2
            049900                    PERFORM PASS                                  IF1044.2
            050000     ELSE                                                         IF1044.2
            050100                    MOVE WS-NUM TO COMPUTED-N                     IF1044.2
            050200                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1044.2
            050300                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1044.2
            050400                    PERFORM FAIL.                                 IF1044.2
            050500     GO TO F-ATAN-WRITE-08.                                       IF1044.2
            050600 F-ATAN-DELETE-08.                                                IF1044.2
            050700     PERFORM  DE-LETE.                                            IF1044.2
            050800     GO TO    F-ATAN-WRITE-08.                                    IF1044.2
            050900 F-ATAN-WRITE-08.                                                 IF1044.2
            051000     MOVE "F-ATAN-08" TO PAR-NAME.                                IF1044.2
            051100     PERFORM  PRINT-DETAIL.                                       IF1044.2
            051200*****************TEST (i) - SIMPLE TEST*****************          IF1044.2
            051300 F-ATAN-09.                                                       IF1044.2
            051400     MOVE ZERO TO WS-NUM.                                         IF1044.2
            051500     MOVE -0.000020 TO MIN-RANGE.                                 IF1044.2
            051600     MOVE  0.000020 TO MAX-RANGE.                                 IF1044.2
            051700 F-ATAN-TEST-09.                                                  IF1044.2
            051800     COMPUTE WS-NUM = FUNCTION ATAN(IND(B)).                      IF1044.2
            051900     IF (WS-NUM >= MIN-RANGE) AND                                 IF1044.2
            052000        (WS-NUM <= MAX-RANGE) THEN                                IF1044.2
            052100                    PERFORM PASS                                  IF1044.2
            052200     ELSE                                                         IF1044.2
            052300                    MOVE WS-NUM TO COMPUTED-N                     IF1044.2
            052400                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1044.2
            052500                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1044.2
            052600                    PERFORM FAIL.                                 IF1044.2
            052700     GO TO F-ATAN-WRITE-09.                                       IF1044.2
            052800 F-ATAN-DELETE-09.                                                IF1044.2
            052900     PERFORM  DE-LETE.                                            IF1044.2
            053000     GO TO    F-ATAN-WRITE-09.                                    IF1044.2
            053100 F-ATAN-WRITE-09.                                                 IF1044.2
            053200     MOVE "F-ATAN-09" TO PAR-NAME.                                IF1044.2
            053300     PERFORM  PRINT-DETAIL.                                       IF1044.2
            053400*****************TEST (a) - COMPLEX TEST****************          IF1044.2
            053500 F-ATAN-10.                                                       IF1044.2
            053600     MOVE ZERO TO WS-NUM.                                         IF1044.2
            053700     MOVE  0.523577  TO MIN-RANGE.                                IF1044.2
            053800     MOVE  0.523619 TO MAX-RANGE.                                 IF1044.2
            053900 F-ATAN-TEST-10.                                                  IF1044.2
            054000     COMPUTE WS-NUM = FUNCTION ATAN(1 / SQRT3).                   IF1044.2
            054100     IF (WS-NUM >= MIN-RANGE) AND                                 IF1044.2
            054200        (WS-NUM <= MAX-RANGE) THEN                                IF1044.2
            054300                    PERFORM PASS                                  IF1044.2
            054400     ELSE                                                         IF1044.2
            054500                    MOVE WS-NUM TO COMPUTED-N                     IF1044.2
            054600                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1044.2
            054700                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1044.2
            054800                    PERFORM FAIL.                                 IF1044.2
            054900     GO TO F-ATAN-WRITE-10.                                       IF1044.2
            055000 F-ATAN-DELETE-10.                                                IF1044.2
            055100     PERFORM  DE-LETE.                                            IF1044.2
            055200     GO TO    F-ATAN-WRITE-10.                                    IF1044.2
            055300 F-ATAN-WRITE-10.                                                 IF1044.2
            055400     MOVE "F-ATAN-10" TO PAR-NAME.                                IF1044.2
            055500     PERFORM  PRINT-DETAIL.                                       IF1044.2
            055600*****************TEST (b) - COMPLEX TEST****************          IF1044.2
            055700 F-ATAN-11.                                                       IF1044.2
            055800     MOVE ZERO TO WS-NUM.                                         IF1044.2
            055900     MOVE  1.04715 TO MIN-RANGE.                                  IF1044.2
            056000     MOVE  1.04723 TO MAX-RANGE.                                  IF1044.2
            056100 F-ATAN-TEST-11.                                                  IF1044.2
            056200     COMPUTE WS-NUM = FUNCTION ATAN(SQRT3).                       IF1044.2
            056300     IF (WS-NUM >= MIN-RANGE) AND                                 IF1044.2
            056400        (WS-NUM <= MAX-RANGE) THEN                                IF1044.2
            056500                    PERFORM PASS                                  IF1044.2
            056600     ELSE                                                         IF1044.2
            056700                    MOVE WS-NUM TO COMPUTED-N                     IF1044.2
            056800                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1044.2
            056900                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1044.2
            057000                    PERFORM FAIL.                                 IF1044.2
            057100     GO TO F-ATAN-WRITE-11.                                       IF1044.2
            057200 F-ATAN-DELETE-11.                                                IF1044.2
            057300     PERFORM  DE-LETE.                                            IF1044.2
            057400     GO TO    F-ATAN-WRITE-11.                                    IF1044.2
            057500 F-ATAN-WRITE-11.                                                 IF1044.2
            057600     MOVE "F-ATAN-11" TO PAR-NAME.                                IF1044.2
            057700     PERFORM  PRINT-DETAIL.                                       IF1044.2
            057800*****************TEST (c) - COMPLEX TEST****************          IF1044.2
            057900 F-ATAN-12.                                                       IF1044.2
            058000     MOVE ZERO TO WS-NUM.                                         IF1044.2
            058100     MOVE  1.04690 TO MIN-RANGE.                                  IF1044.2
            058200     MOVE  1.04698 TO MAX-RANGE.                                  IF1044.2
            058300 F-ATAN-TEST-12.                                                  IF1044.2
            058400     COMPUTE WS-NUM = FUNCTION ATAN(SQRT3 - .001).                IF1044.2
            058500     IF (WS-NUM >= MIN-RANGE) AND                                 IF1044.2
            058600        (WS-NUM <= MAX-RANGE) THEN                                IF1044.2
            058700                    PERFORM PASS                                  IF1044.2
            058800     ELSE                                                         IF1044.2
            058900                    MOVE WS-NUM TO COMPUTED-N                     IF1044.2
            059000                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1044.2
            059100                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1044.2
            059200                    PERFORM FAIL.                                 IF1044.2
            059300     GO TO F-ATAN-WRITE-12.                                       IF1044.2
            059400 F-ATAN-DELETE-12.                                                IF1044.2
            059500     PERFORM  DE-LETE.                                            IF1044.2
            059600     GO TO    F-ATAN-WRITE-12.                                    IF1044.2
            059700 F-ATAN-WRITE-12.                                                 IF1044.2
            059800     MOVE "F-ATAN-12" TO PAR-NAME.                                IF1044.2
            059900     PERFORM  PRINT-DETAIL.                                       IF1044.2
            060000*****************TEST (d) - COMPLEX TEST****************          IF1044.2
            060100 F-ATAN-13.                                                       IF1044.2
            060200     MOVE ZERO TO WS-NUM.                                         IF1044.2
            060300     MOVE  0.522827 TO MIN-RANGE.                                 IF1044.2
            060400     MOVE  0.522869 TO MAX-RANGE.                                 IF1044.2
            060500 F-ATAN-TEST-13.                                                  IF1044.2
            060600     COMPUTE WS-NUM = FUNCTION ATAN((1 / SQRT3) - .001).          IF1044.2
            060700     IF (WS-NUM >= MIN-RANGE) AND                                 IF1044.2
            060800        (WS-NUM <= MAX-RANGE) THEN                                IF1044.2
            060900                    PERFORM PASS                                  IF1044.2
            061000     ELSE                                                         IF1044.2
            061100                    MOVE WS-NUM TO COMPUTED-N                     IF1044.2
            061200                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1044.2
            061300                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1044.2
            061400                    PERFORM FAIL.                                 IF1044.2
            061500     GO TO F-ATAN-WRITE-13.                                       IF1044.2
            061600 F-ATAN-DELETE-13.                                                IF1044.2
            061700     PERFORM  DE-LETE.                                            IF1044.2
            061800     GO TO    F-ATAN-WRITE-13.                                    IF1044.2
            061900 F-ATAN-WRITE-13.                                                 IF1044.2
            062000     MOVE "F-ATAN-13" TO PAR-NAME.                                IF1044.2
            062100     PERFORM  PRINT-DETAIL.                                       IF1044.2
            062200*****************TEST (e) - COMPLEX TEST****************          IF1044.2
            062300 F-ATAN-14.                                                       IF1044.2
            062400     MOVE ZERO TO WS-NUM.                                         IF1044.2
            062500     MOVE -0.010000  TO MIN-RANGE.                                IF1044.2
            062600     MOVE -0.009998 TO MAX-RANGE.                                 IF1044.2
            062700 F-ATAN-TEST-14.                                                  IF1044.2
            062800     COMPUTE WS-NUM = FUNCTION ATAN( 1 - 1.01).                   IF1044.2
            062900     IF (WS-NUM >= MIN-RANGE) AND                                 IF1044.2
            063000        (WS-NUM <= MAX-RANGE) THEN                                IF1044.2
            063100                    PERFORM PASS                                  IF1044.2
            063200     ELSE                                                         IF1044.2
            063300                    MOVE WS-NUM TO COMPUTED-N                     IF1044.2
            063400                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1044.2
            063500                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1044.2
            063600                    PERFORM FAIL.                                 IF1044.2
            063700     GO TO F-ATAN-WRITE-14.                                       IF1044.2
            063800 F-ATAN-DELETE-14.                                                IF1044.2
            063900     PERFORM  DE-LETE.                                            IF1044.2
            064000     GO TO    F-ATAN-WRITE-14.                                    IF1044.2
            064100 F-ATAN-WRITE-14.                                                 IF1044.2
            064200     MOVE "F-ATAN-14" TO PAR-NAME.                                IF1044.2
            064300     PERFORM  PRINT-DETAIL.                                       IF1044.2
            064400*****************TEST (f) - COMPLEX TEST****************          IF1044.2
            064500 F-ATAN-15.                                                       IF1044.2
            064600     MOVE ZERO TO WS-NUM.                                         IF1044.2
            064700     MOVE  0.780342  TO MIN-RANGE.                                IF1044.2
            064800     MOVE  0.780404 TO MAX-RANGE.                                 IF1044.2
            064900 F-ATAN-TEST-15.                                                  IF1044.2
            065000     COMPUTE WS-NUM = FUNCTION ATAN(1.98 / 2).                    IF1044.2
            065100     IF (WS-NUM >= MIN-RANGE) AND                                 IF1044.2
            065200        (WS-NUM <= MAX-RANGE) THEN                                IF1044.2
            065300                    PERFORM PASS                                  IF1044.2
            065400     ELSE                                                         IF1044.2
            065500                    MOVE WS-NUM TO COMPUTED-N                     IF1044.2
            065600                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1044.2
            065700                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1044.2
            065800                    PERFORM FAIL.                                 IF1044.2
            065900     GO TO F-ATAN-WRITE-15.                                       IF1044.2
            066000 F-ATAN-DELETE-15.                                                IF1044.2
            066100     PERFORM  DE-LETE.                                            IF1044.2
            066200     GO TO    F-ATAN-WRITE-15.                                    IF1044.2
            066300 F-ATAN-WRITE-15.                                                 IF1044.2
            066400     MOVE "F-ATAN-15" TO PAR-NAME.                                IF1044.2
            066500     PERFORM  PRINT-DETAIL.                                       IF1044.2
            066600*****************TEST (g) - COMPLEX TEST****************          IF1044.2
            066700 F-ATAN-16.                                                       IF1044.2
            066800     MOVE ZERO TO WS-NUM.                                         IF1044.2
            066900     MOVE  1.04964 TO MIN-RANGE.                                  IF1044.2
            067000     MOVE  1.04972 TO MAX-RANGE.                                  IF1044.2
            067100 F-ATAN-TEST-16.                                                  IF1044.2
            067200     COMPUTE WS-NUM = FUNCTION ATAN(SQRT3 + .01).                 IF1044.2
            067300     IF (WS-NUM >= MIN-RANGE) AND                                 IF1044.2
            067400        (WS-NUM <= MAX-RANGE) THEN                                IF1044.2
            067500                    PERFORM PASS                                  IF1044.2
            067600     ELSE                                                         IF1044.2
            067700                    MOVE WS-NUM TO COMPUTED-N                     IF1044.2
            067800                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1044.2
            067900                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1044.2
            068000                    PERFORM FAIL.                                 IF1044.2
            068100     GO TO F-ATAN-WRITE-16.                                       IF1044.2
            068200 F-ATAN-DELETE-16.                                                IF1044.2
            068300     PERFORM  DE-LETE.                                            IF1044.2
            068400     GO TO    F-ATAN-WRITE-16.                                    IF1044.2
            068500 F-ATAN-WRITE-16.                                                 IF1044.2
            068600     MOVE "F-ATAN-16" TO PAR-NAME.                                IF1044.2
            068700     PERFORM  PRINT-DETAIL.                                       IF1044.2
            068800*****************TEST (h) - COMPLEX TEST****************          IF1044.2
            068900 F-ATAN-17.                                                       IF1044.2
            069000     MOVE ZERO TO WS-NUM.                                         IF1044.2
            069100     MOVE  0.531045 TO MIN-RANGE.                                 IF1044.2
            069200     MOVE  0.531087 TO MAX-RANGE.                                 IF1044.2
            069300 F-ATAN-TEST-17.                                                  IF1044.2
            069400     COMPUTE WS-NUM = FUNCTION ATAN((1 / SQRT3) + .01).           IF1044.2
            069500     IF (WS-NUM >= MIN-RANGE) AND                                 IF1044.2
            069600        (WS-NUM <= MAX-RANGE) THEN                                IF1044.2
            069700                    PERFORM PASS                                  IF1044.2
            069800     ELSE                                                         IF1044.2
            069900                    MOVE WS-NUM TO COMPUTED-N                     IF1044.2
            070000                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1044.2
            070100                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1044.2
            070200                    PERFORM FAIL.                                 IF1044.2
            070300     GO TO F-ATAN-WRITE-17.                                       IF1044.2
            070400 F-ATAN-DELETE-17.                                                IF1044.2
            070500     PERFORM  DE-LETE.                                            IF1044.2
            070600     GO TO    F-ATAN-WRITE-17.                                    IF1044.2
            070700 F-ATAN-WRITE-17.                                                 IF1044.2
            070800     MOVE "F-ATAN-17" TO PAR-NAME.                                IF1044.2
            070900     PERFORM  PRINT-DETAIL.                                       IF1044.2
            071000*****************TEST (i) - COMPLEX TEST****************          IF1044.2
            071100 F-ATAN-18.                                                       IF1044.2
            071200     MOVE ZERO TO WS-NUM.                                         IF1044.2
            071300     MOVE  1.19023 TO MIN-RANGE.                                  IF1044.2
            071400     MOVE  1.19033 TO MAX-RANGE.                                  IF1044.2
            071500 F-ATAN-TEST-18.                                                  IF1044.2
            071600     COMPUTE WS-NUM = FUNCTION ATAN(IND(3) / B).                  IF1044.2
            071700     IF (WS-NUM >= MIN-RANGE) AND                                 IF1044.2
            071800        (WS-NUM <= MAX-RANGE) THEN                                IF1044.2
            071900                    PERFORM PASS                                  IF1044.2
            072000     ELSE                                                         IF1044.2
            072100                    MOVE WS-NUM TO COMPUTED-N                     IF1044.2
            072200                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1044.2
            072300                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1044.2
            072400                    PERFORM FAIL.                                 IF1044.2
            072500     GO TO F-ATAN-WRITE-18.                                       IF1044.2
            072600 F-ATAN-DELETE-18.                                                IF1044.2
            072700     PERFORM  DE-LETE.                                            IF1044.2
            072800     GO TO    F-ATAN-WRITE-18.                                    IF1044.2
            072900 F-ATAN-WRITE-18.                                                 IF1044.2
            073000     MOVE "F-ATAN-18" TO PAR-NAME.                                IF1044.2
            073100     PERFORM  PRINT-DETAIL.                                       IF1044.2
            073200*****************TEST (j) - COMPLEX TEST****************          IF1044.2
            073300 F-ATAN-19.                                                       IF1044.2
            073400     MOVE ZERO TO WS-NUM.                                         IF1044.2
            073500     MOVE  0.785367 TO MIN-RANGE.                                 IF1044.2
            073600     MOVE  0.785429 TO MAX-RANGE.                                 IF1044.2
            073700 F-ATAN-TEST-19.                                                  IF1044.2
            073800     COMPUTE WS-NUM = FUNCTION ATAN(4 - 3).                       IF1044.2
            073900     IF (WS-NUM >= MIN-RANGE) AND                                 IF1044.2
            074000        (WS-NUM <= MAX-RANGE) THEN                                IF1044.2
            074100                    PERFORM PASS                                  IF1044.2
            074200     ELSE                                                         IF1044.2
            074300                    MOVE WS-NUM TO COMPUTED-N                     IF1044.2
            074400                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1044.2
            074500                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1044.2
            074600                    PERFORM FAIL.                                 IF1044.2
            074700     GO TO F-ATAN-WRITE-19.                                       IF1044.2
            074800 F-ATAN-DELETE-19.                                                IF1044.2
            074900     PERFORM  DE-LETE.                                            IF1044.2
            075000     GO TO    F-ATAN-WRITE-19.                                    IF1044.2
            075100 F-ATAN-WRITE-19.                                                 IF1044.2
            075200     MOVE "F-ATAN-19" TO PAR-NAME.                                IF1044.2
            075300     PERFORM  PRINT-DETAIL.                                       IF1044.2
            075400*****************TEST (k) - COMPLEX TEST****************          IF1044.2
            075500 F-ATAN-20.                                                       IF1044.2
            075600     MOVE ZERO TO WS-NUM.                                         IF1044.2
            075700     MOVE -0.000040 TO MIN-RANGE.                                 IF1044.2
            075800     MOVE  0.000040 TO MAX-RANGE.                                 IF1044.2
            075900 F-ATAN-TEST-20.                                                  IF1044.2
            076000     COMPUTE WS-NUM = FUNCTION ATAN(C - C).                       IF1044.2
            076100     IF (WS-NUM >= MIN-RANGE) AND                                 IF1044.2
            076200        (WS-NUM <= MAX-RANGE) THEN                                IF1044.2
            076300                    PERFORM PASS                                  IF1044.2
            076400     ELSE                                                         IF1044.2
            076500                    MOVE WS-NUM TO COMPUTED-N                     IF1044.2
            076600                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1044.2
            076700                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1044.2
            076800                    PERFORM FAIL.                                 IF1044.2
            076900     GO TO F-ATAN-WRITE-20.                                       IF1044.2
            077000 F-ATAN-DELETE-20.                                                IF1044.2
            077100     PERFORM  DE-LETE.                                            IF1044.2
            077200     GO TO    F-ATAN-WRITE-20.                                    IF1044.2
            077300 F-ATAN-WRITE-20.                                                 IF1044.2
            077400     MOVE "F-ATAN-20" TO PAR-NAME.                                IF1044.2
            077500     PERFORM  PRINT-DETAIL.                                       IF1044.2
            077600*****************TEST (l) - COMPLEX TEST****************          IF1044.2
            077700 F-ATAN-21.                                                       IF1044.2
            077800     MOVE ZERO TO WS-NUM.                                         IF1044.2
            077900     MOVE  0.244968  TO MIN-RANGE.                                IF1044.2
            078000     MOVE  0.244988 TO MAX-RANGE.                                 IF1044.2
            078100 F-ATAN-TEST-21.                                                  IF1044.2
            078200     COMPUTE WS-NUM = FUNCTION ATAN(0.25 * 1).                    IF1044.2
            078300     IF (WS-NUM >= MIN-RANGE) AND                                 IF1044.2
            078400        (WS-NUM <= MAX-RANGE) THEN                                IF1044.2
            078500                    PERFORM PASS                                  IF1044.2
            078600     ELSE                                                         IF1044.2
            078700                    MOVE WS-NUM TO COMPUTED-N                     IF1044.2
            078800                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1044.2
            078900                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1044.2
            079000                    PERFORM FAIL.                                 IF1044.2
            079100     GO TO F-ATAN-WRITE-21.                                       IF1044.2
            079200 F-ATAN-DELETE-21.                                                IF1044.2
            079300     PERFORM  DE-LETE.                                            IF1044.2
            079400     GO TO    F-ATAN-WRITE-21.                                    IF1044.2
            079500 F-ATAN-WRITE-21.                                                 IF1044.2
            079600     MOVE "F-ATAN-21" TO PAR-NAME.                                IF1044.2
            079700     PERFORM  PRINT-DETAIL.                                       IF1044.2
            079800*****************TEST (m) - COMPLEX TEST****************          IF1044.2
            079900 F-ATAN-22.                                                       IF1044.2
            080000     MOVE ZERO TO WS-NUM.                                         IF1044.2
            080100     MOVE  0.308157  TO MIN-RANGE.                                IF1044.2
            080200     MOVE  0.308181 TO MAX-RANGE.                                 IF1044.2
            080300 F-ATAN-TEST-22.                                                  IF1044.2
            080400     COMPUTE WS-NUM = FUNCTION ATAN(1 / PI).                      IF1044.2
            080500     IF (WS-NUM >= MIN-RANGE) AND                                 IF1044.2
            080600        (WS-NUM <= MAX-RANGE) THEN                                IF1044.2
            080700                    PERFORM PASS                                  IF1044.2
            080800     ELSE                                                         IF1044.2
            080900                    MOVE WS-NUM TO COMPUTED-N                     IF1044.2
            081000                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1044.2
            081100                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1044.2
            081200                    PERFORM FAIL.                                 IF1044.2
            081300     GO TO F-ATAN-WRITE-22.                                       IF1044.2
            081400 F-ATAN-DELETE-22.                                                IF1044.2
            081500     PERFORM  DE-LETE.                                            IF1044.2
            081600     GO TO    F-ATAN-WRITE-22.                                    IF1044.2
            081700 F-ATAN-WRITE-22.                                                 IF1044.2
            081800     MOVE "F-ATAN-22" TO PAR-NAME.                                IF1044.2
            081900     PERFORM  PRINT-DETAIL.                                       IF1044.2
            082000*****************TEST (n) - COMPLEX TEST****************          IF1044.2
            082100 F-ATAN-23.                                                       IF1044.2
            082200     MOVE ZERO TO WS-NUM.                                         IF1044.2
            082300     MOVE -0.000040 TO MIN-RANGE.                                 IF1044.2
            082400     MOVE  0.000040 TO MAX-RANGE.                                 IF1044.2
            082500 F-ATAN-TEST-23.                                                  IF1044.2
            082600     COMPUTE WS-NUM = FUNCTION ATAN((D / D) - 1).                 IF1044.2
            082700     IF (WS-NUM >= MIN-RANGE) AND                                 IF1044.2
            082800        (WS-NUM <= MAX-RANGE) THEN                                IF1044.2
            082900                    PERFORM PASS                                  IF1044.2
            083000     ELSE                                                         IF1044.2
            083100                    MOVE WS-NUM TO COMPUTED-N                     IF1044.2
            083200                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1044.2
            083300                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1044.2
            083400                    PERFORM FAIL.                                 IF1044.2
            083500     GO TO F-ATAN-WRITE-23.                                       IF1044.2
            083600 F-ATAN-DELETE-23.                                                IF1044.2
            083700     PERFORM  DE-LETE.                                            IF1044.2
            083800     GO TO    F-ATAN-WRITE-23.                                    IF1044.2
            083900 F-ATAN-WRITE-23.                                                 IF1044.2
            084000     MOVE "F-ATAN-23" TO PAR-NAME.                                IF1044.2
            084100     PERFORM  PRINT-DETAIL.                                       IF1044.2
            084200*****************TEST (o) - COMPLEX TEST****************          IF1044.2
            084300 F-ATAN-24.                                                       IF1044.2
            084400     MOVE ZERO TO WS-NUM.                                         IF1044.2
            084500     MOVE -0.709382 TO MIN-RANGE.                                 IF1044.2
            084600     MOVE -0.709326 TO MAX-RANGE.                                 IF1044.2
            084700 F-ATAN-TEST-24.                                                  IF1044.2
            084800     COMPUTE WS-NUM = FUNCTION ATAN(PI - 4).                      IF1044.2
            084900     IF (WS-NUM >= MIN-RANGE) AND                                 IF1044.2
            085000        (WS-NUM <= MAX-RANGE) THEN                                IF1044.2
            085100                    PERFORM PASS                                  IF1044.2
            085200     ELSE                                                         IF1044.2
            085300                    MOVE WS-NUM TO COMPUTED-N                     IF1044.2
            085400                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1044.2
            085500                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1044.2
            085600                    PERFORM FAIL.                                 IF1044.2
            085700     GO TO F-ATAN-WRITE-24.                                       IF1044.2
            085800 F-ATAN-DELETE-24.                                                IF1044.2
            085900     PERFORM  DE-LETE.                                            IF1044.2
            086000     GO TO    F-ATAN-WRITE-24.                                    IF1044.2
            086100 F-ATAN-WRITE-24.                                                 IF1044.2
            086200     MOVE "F-ATAN-24" TO PAR-NAME.                                IF1044.2
            086300     PERFORM  PRINT-DETAIL.                                       IF1044.2
            086400*****************TEST (p) - COMPLEX TEST****************          IF1044.2
            086500 F-ATAN-25.                                                       IF1044.2
            086600     MOVE ZERO TO WS-NUM.                                         IF1044.2
            086700     MOVE 0.511215 TO MIN-RANGE.                                  IF1044.2
            086800     MOVE 0.511255 TO MAX-RANGE.                                  IF1044.2
            086900 F-ATAN-TEST-25.                                                  IF1044.2
            087000     COMPUTE WS-NUM = FUNCTION ATAN(FUNCTION ATAN(PI / 5)).       IF1044.2
            087100     IF (WS-NUM >= MIN-RANGE) AND                                 IF1044.2
            087200        (WS-NUM <= MAX-RANGE) THEN                                IF1044.2
            087300                    PERFORM PASS                                  IF1044.2
            087400     ELSE                                                         IF1044.2
            087500                    MOVE WS-NUM TO COMPUTED-N                     IF1044.2
            087600                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1044.2
            087700                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1044.2
            087800                    PERFORM FAIL.                                 IF1044.2
            087900     GO TO F-ATAN-WRITE-25.                                       IF1044.2
            088000 F-ATAN-DELETE-25.                                                IF1044.2
            088100     PERFORM  DE-LETE.                                            IF1044.2
            088200     GO TO    F-ATAN-WRITE-25.                                    IF1044.2
            088300 F-ATAN-WRITE-25.                                                 IF1044.2
            088400     MOVE "F-ATAN-25" TO PAR-NAME.                                IF1044.2
            088500     PERFORM  PRINT-DETAIL.                                       IF1044.2
            088600*****************TEST (q) - COMPLEX TEST****************          IF1044.2
            088700 F-ATAN-26.                                                       IF1044.2
            088800     MOVE ZERO TO WS-NUM.                                         IF1044.2
            088900     MOVE -0.000040 TO MIN-RANGE.                                 IF1044.2
            089000     MOVE 0.000040 TO MAX-RANGE.                                  IF1044.2
            089100 F-ATAN-TEST-26.                                                  IF1044.2
            089200     COMPUTE WS-NUM = FUNCTION ATAN(0.6) + FUNCTION ATAN(-0.6).   IF1044.2
            089300                                                                  IF1044.2
            089400     IF (WS-NUM >= MIN-RANGE) AND                                 IF1044.2
            089500        (WS-NUM <= MAX-RANGE) THEN                                IF1044.2
            089600                    PERFORM PASS                                  IF1044.2
            089700     ELSE                                                         IF1044.2
            089800                    MOVE WS-NUM TO COMPUTED-N                     IF1044.2
            089900                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1044.2
            090000                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1044.2
            090100                    PERFORM FAIL.                                 IF1044.2
            090200     GO TO F-ATAN-WRITE-26.                                       IF1044.2
            090300 F-ATAN-DELETE-26.                                                IF1044.2
            090400     PERFORM  DE-LETE.                                            IF1044.2
            090500     GO TO    F-ATAN-WRITE-26.                                    IF1044.2
            090600 F-ATAN-WRITE-26.                                                 IF1044.2
            090700     MOVE "F-ATAN-26" TO PAR-NAME.                                IF1044.2
            090800     PERFORM  PRINT-DETAIL.                                       IF1044.2
            090900*****************SPECIAL PERFORM TEST**********************       IF1044.2
            091000 F-ATAN-27.                                                       IF1044.2
            091100     MOVE ZERO TO WS-NUM.                                         IF1044.2
            091200     PERFORM F-ATAN-TEST-27                                       IF1044.2
            091300       UNTIL FUNCTION ATAN(ARG1) < 0.                             IF1044.2
            091400     PERFORM PASS.                                                IF1044.2
            091500     GO TO F-ATAN-WRITE-27.                                       IF1044.2
            091600 F-ATAN-TEST-27.                                                  IF1044.2
            091700     COMPUTE ARG1 = ARG1 - 0.25.                                  IF1044.2
            091800 F-ATAN-DELETE-27.                                                IF1044.2
            091900     PERFORM  DE-LETE.                                            IF1044.2
            092000     GO TO    F-ATAN-WRITE-27.                                    IF1044.2
            092100 F-ATAN-WRITE-27.                                                 IF1044.2
            092200     MOVE "F-ATAN-27" TO PAR-NAME.                                IF1044.2
            092300     PERFORM  PRINT-DETAIL.                                       IF1044.2
            092400********************END OF TESTS***************                   IF1044.2
            092500 CCVS-EXIT SECTION.                                               IF1044.2
            092600 CCVS-999999.                                                     IF1044.2
            092700     GO TO CLOSE-FILES.                                           IF1044.2
                  *END-OF,IF104A                                                                  
        """)
    )

    @Test
    fun if1054_2() = rewriteRun(
        cobol("""
                  *HEADER,COBOL,IF105A                                                            
            000100 IDENTIFICATION DIVISION.                                         IF1054.2
            000200 PROGRAM-ID.                                                      IF1054.2
            000300     IF105A.                                                      IF1054.2
            000400                                                                  IF1054.2
            000500***********************************************************       IF1054.2
            000600*                                                         *       IF1054.2
            000700* This program forms part of the CCVS85 COBOL Test Suite. *       IF1054.2
            000800* It contains tests for the Intrinsic Function CHAR.      *       IF1054.2
            000900*                                                         *       IF1054.2
            001000*                                                         *       IF1054.2
            001100***********************************************************       IF1054.2
            001200 ENVIRONMENT DIVISION.                                            IF1054.2
            001300 CONFIGURATION SECTION.                                           IF1054.2
            001400 SOURCE-COMPUTER.                                                 IF1054.2
            001500     XXXXX082.                                                    IF1054.2
            001600 OBJECT-COMPUTER.                                                 IF1054.2
            001700     XXXXX083                                                     IF1054.2
            001800     PROGRAM COLLATING SEQUENCE IS PRG-COLL-SEQ.                  IF1054.2
            001900 SPECIAL-NAMES.                                                   IF1054.2
            002000     ALPHABET PRG-COLL-SEQ IS                                     IF1054.2
            002100     STANDARD-2.                                                  IF1054.2
            002200 INPUT-OUTPUT SECTION.                                            IF1054.2
            002300 FILE-CONTROL.                                                    IF1054.2
            002400     SELECT PRINT-FILE ASSIGN TO                                  IF1054.2
            002500     XXXXX055.                                                    IF1054.2
            002600 DATA DIVISION.                                                   IF1054.2
            002700 FILE SECTION.                                                    IF1054.2
            002800 FD  PRINT-FILE.                                                  IF1054.2
            002900 01  PRINT-REC PICTURE X(120).                                    IF1054.2
            003000 01  DUMMY-RECORD PICTURE X(120).                                 IF1054.2
            003100 WORKING-STORAGE SECTION.                                         IF1054.2
            003200***********************************************************       IF1054.2
            003300* Variables specific to the Intrinsic Function Test IF105A*       IF1054.2
            003400***********************************************************       IF1054.2
            003500 01  B                   PIC S9(10)     VALUE 37.                 IF1054.2
            003600 01  C                   PIC S9(10)     VALUE 2.                  IF1054.2
            003700 01  D                   PIC S9(10)     VALUE 100.                IF1054.2
            003800 01  ARR                 VALUE "066037100070044".                 IF1054.2
            003900     02 IND OCCURS 5 TIMES PIC 9(3).                              IF1054.2
            004000 01  TEMP                PIC S9(5)V9(5).                          IF1054.2
            004100 01  WS-ANUM          PIC X.                                      IF1054.2
            004200*                                                                 IF1054.2
            004300**********************************************************        IF1054.2
            004400*                                                                 IF1054.2
            004500 01  TEST-RESULTS.                                                IF1054.2
            004600     02 FILLER                   PIC X      VALUE SPACE.          IF1054.2
            004700     02 FEATURE                  PIC X(20)  VALUE SPACE.          IF1054.2
            004800     02 FILLER                   PIC X      VALUE SPACE.          IF1054.2
            004900     02 P-OR-F                   PIC X(5)   VALUE SPACE.          IF1054.2
            005000     02 FILLER                   PIC X      VALUE SPACE.          IF1054.2
            005100     02  PAR-NAME.                                                IF1054.2
            005200       03 FILLER                 PIC X(19)  VALUE SPACE.          IF1054.2
            005300       03  PARDOT-X              PIC X      VALUE SPACE.          IF1054.2
            005400       03 DOTVALUE               PIC 99     VALUE ZERO.           IF1054.2
            005500     02 FILLER                   PIC X(8)   VALUE SPACE.          IF1054.2
            005600     02 RE-MARK                  PIC X(61).                       IF1054.2
            005700 01  TEST-COMPUTED.                                               IF1054.2
            005800     02 FILLER                   PIC X(30)  VALUE SPACE.          IF1054.2
            005900     02 FILLER                   PIC X(17)  VALUE                 IF1054.2
            006000            "       COMPUTED=".                                   IF1054.2
            006100     02 COMPUTED-X.                                               IF1054.2
            006200     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          IF1054.2
            006300     03 COMPUTED-N               REDEFINES COMPUTED-A             IF1054.2
            006400                                 PIC -9(9).9(9).                  IF1054.2
            006500     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         IF1054.2
            006600     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     IF1054.2
            006700     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     IF1054.2
            006800     03       CM-18V0 REDEFINES COMPUTED-A.                       IF1054.2
            006900         04 COMPUTED-18V0                    PIC -9(18).          IF1054.2
            007000         04 FILLER                           PIC X.               IF1054.2
            007100     03 FILLER PIC X(50) VALUE SPACE.                             IF1054.2
            007200 01  TEST-CORRECT.                                                IF1054.2
            007300     02 FILLER PIC X(30) VALUE SPACE.                             IF1054.2
            007400     02 FILLER PIC X(17) VALUE "       CORRECT =".                IF1054.2
            007500     02 CORRECT-X.                                                IF1054.2
            007600     03 CORRECT-A                  PIC X(20) VALUE SPACE.         IF1054.2
            007700     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      IF1054.2
            007800     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         IF1054.2
            007900     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     IF1054.2
            008000     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     IF1054.2
            008100     03      CR-18V0 REDEFINES CORRECT-A.                         IF1054.2
            008200         04 CORRECT-18V0                     PIC -9(18).          IF1054.2
            008300         04 FILLER                           PIC X.               IF1054.2
            008400     03 FILLER PIC X(2) VALUE SPACE.                              IF1054.2
            008500     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     IF1054.2
            008600 01  TEST-CORRECT-MIN.                                            IF1054.2
            008700     02 FILLER PIC X(30) VALUE SPACE.                             IF1054.2
            008800     02 FILLER PIC X(17) VALUE "     MIN VALUE =".                IF1054.2
            008900     02 CORRECTMI-X.                                              IF1054.2
            009000     03 CORRECTMI-A                 PIC X(20) VALUE SPACE.        IF1054.2
            009100     03 CORRECT-MIN    REDEFINES CORRECTMI-A     PIC -9(9).9(9).  IF1054.2
            009200     03 CORRECTMI-0V18 REDEFINES CORRECTMI-A     PIC -.9(18).     IF1054.2
            009300     03 CORRECTMI-4V14 REDEFINES CORRECTMI-A     PIC -9(4).9(14). IF1054.2
            009400     03 CORRECTMI-14V4 REDEFINES CORRECTMI-A     PIC -9(14).9(4). IF1054.2
            009500     03      CR-18V0 REDEFINES CORRECTMI-A.                       IF1054.2
            009600         04 CORRECTMI-18V0                     PIC -9(18).        IF1054.2
            009700         04 FILLER                           PIC X.               IF1054.2
            009800     03 FILLER PIC X(2) VALUE SPACE.                              IF1054.2
            009900     03 FILLER                           PIC X(48) VALUE SPACE.   IF1054.2
            010000 01  TEST-CORRECT-MAX.                                            IF1054.2
            010100     02 FILLER PIC X(30) VALUE SPACE.                             IF1054.2
            010200     02 FILLER PIC X(17) VALUE "     MAX VALUE =".                IF1054.2
            010300     02 CORRECTMA-X.                                              IF1054.2
            010400     03 CORRECTMA-A                  PIC X(20) VALUE SPACE.       IF1054.2
            010500     03 CORRECT-MAX    REDEFINES CORRECTMA-A     PIC -9(9).9(9).  IF1054.2
            010600     03 CORRECTMA-0V18 REDEFINES CORRECTMA-A     PIC -.9(18).     IF1054.2
            010700     03 CORRECTMA-4V14 REDEFINES CORRECTMA-A     PIC -9(4).9(14). IF1054.2
            010800     03 CORRECTMA-14V4 REDEFINES CORRECTMA-A     PIC -9(14).9(4). IF1054.2
            010900     03      CR-18V0 REDEFINES CORRECTMA-A.                       IF1054.2
            011000         04 CORRECTMA-18V0                     PIC -9(18).        IF1054.2
            011100         04 FILLER                           PIC X.               IF1054.2
            011200     03 FILLER PIC X(2) VALUE SPACE.                              IF1054.2
            011300     03 CORMA-ANSI-REFERENCE             PIC X(48) VALUE SPACE.   IF1054.2
            011400 01  CCVS-C-1.                                                    IF1054.2
            011500     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PAIF1054.2
            011600-    "SS  PARAGRAPH-NAME                                          IF1054.2
            011700-    "       REMARKS".                                            IF1054.2
            011800     02 FILLER                     PIC X(20)    VALUE SPACE.      IF1054.2
            011900 01  CCVS-C-2.                                                    IF1054.2
            012000     02 FILLER                     PIC X        VALUE SPACE.      IF1054.2
            012100     02 FILLER                     PIC X(6)     VALUE "TESTED".   IF1054.2
            012200     02 FILLER                     PIC X(15)    VALUE SPACE.      IF1054.2
            012300     02 FILLER                     PIC X(4)     VALUE "FAIL".     IF1054.2
            012400     02 FILLER                     PIC X(94)    VALUE SPACE.      IF1054.2
            012500 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       IF1054.2
            012600 01  REC-CT                        PIC 99       VALUE ZERO.       IF1054.2
            012700 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       IF1054.2
            012800 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       IF1054.2
            012900 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       IF1054.2
            013000 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       IF1054.2
            013100 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       IF1054.2
            013200 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       IF1054.2
            013300 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      IF1054.2
            013400 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       IF1054.2
            013500 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     IF1054.2
            013600 01  CCVS-H-1.                                                    IF1054.2
            013700     02  FILLER                    PIC X(39)    VALUE SPACES.     IF1054.2
            013800     02  FILLER                    PIC X(42)    VALUE             IF1054.2
            013900     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 IF1054.2
            014000     02  FILLER                    PIC X(39)    VALUE SPACES.     IF1054.2
            014100 01  CCVS-H-2A.                                                   IF1054.2
            014200   02  FILLER                        PIC X(40)  VALUE SPACE.      IF1054.2
            014300   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  IF1054.2
            014400   02  FILLER                        PIC XXXX   VALUE             IF1054.2
            014500     "4.2 ".                                                      IF1054.2
            014600   02  FILLER                        PIC X(28)  VALUE             IF1054.2
            014700            " COPY - NOT FOR DISTRIBUTION".                       IF1054.2
            014800   02  FILLER                        PIC X(41)  VALUE SPACE.      IF1054.2
            014900                                                                  IF1054.2
            015000 01  CCVS-H-2B.                                                   IF1054.2
            015100   02  FILLER                        PIC X(15)  VALUE             IF1054.2
            015200            "TEST RESULT OF ".                                    IF1054.2
            015300   02  TEST-ID                       PIC X(9).                    IF1054.2
            015400   02  FILLER                        PIC X(4)   VALUE             IF1054.2
            015500            " IN ".                                               IF1054.2
            015600   02  FILLER                        PIC X(12)  VALUE             IF1054.2
            015700     " HIGH       ".                                              IF1054.2
            015800   02  FILLER                        PIC X(22)  VALUE             IF1054.2
            015900            " LEVEL VALIDATION FOR ".                             IF1054.2
            016000   02  FILLER                        PIC X(58)  VALUE             IF1054.2
            016100     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".IF1054.2
            016200 01  CCVS-H-3.                                                    IF1054.2
            016300     02  FILLER                      PIC X(34)  VALUE             IF1054.2
            016400            " FOR OFFICIAL USE ONLY    ".                         IF1054.2
            016500     02  FILLER                      PIC X(58)  VALUE             IF1054.2
            016600     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".IF1054.2
            016700     02  FILLER                      PIC X(28)  VALUE             IF1054.2
            016800            "  COPYRIGHT   1985 ".                                IF1054.2
            016900 01  CCVS-E-1.                                                    IF1054.2
            017000     02 FILLER                       PIC X(52)  VALUE SPACE.      IF1054.2
            017100     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              IF1054.2
            017200     02 ID-AGAIN                     PIC X(9).                    IF1054.2
            017300     02 FILLER                       PIC X(45)  VALUE SPACES.     IF1054.2
            017400 01  CCVS-E-2.                                                    IF1054.2
            017500     02  FILLER                      PIC X(31)  VALUE SPACE.      IF1054.2
            017600     02  FILLER                      PIC X(21)  VALUE SPACE.      IF1054.2
            017700     02 CCVS-E-2-2.                                               IF1054.2
            017800         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      IF1054.2
            017900         03 FILLER                   PIC X      VALUE SPACE.      IF1054.2
            018000         03 ENDER-DESC               PIC X(44)  VALUE             IF1054.2
            018100            "ERRORS ENCOUNTERED".                                 IF1054.2
            018200 01  CCVS-E-3.                                                    IF1054.2
            018300     02  FILLER                      PIC X(22)  VALUE             IF1054.2
            018400            " FOR OFFICIAL USE ONLY".                             IF1054.2
            018500     02  FILLER                      PIC X(12)  VALUE SPACE.      IF1054.2
            018600     02  FILLER                      PIC X(58)  VALUE             IF1054.2
            018700     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".IF1054.2
            018800     02  FILLER                      PIC X(13)  VALUE SPACE.      IF1054.2
            018900     02 FILLER                       PIC X(15)  VALUE             IF1054.2
            019000             " COPYRIGHT 1985".                                   IF1054.2
            019100 01  CCVS-E-4.                                                    IF1054.2
            019200     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      IF1054.2
            019300     02 FILLER                       PIC X(4)   VALUE " OF ".     IF1054.2
            019400     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      IF1054.2
            019500     02 FILLER                       PIC X(40)  VALUE             IF1054.2
            019600      "  TESTS WERE EXECUTED SUCCESSFULLY".                       IF1054.2
            019700 01  XXINFO.                                                      IF1054.2
            019800     02 FILLER                       PIC X(19)  VALUE             IF1054.2
            019900            "*** INFORMATION ***".                                IF1054.2
            020000     02 INFO-TEXT.                                                IF1054.2
            020100       04 FILLER                     PIC X(8)   VALUE SPACE.      IF1054.2
            020200       04 XXCOMPUTED                 PIC X(20).                   IF1054.2
            020300       04 FILLER                     PIC X(5)   VALUE SPACE.      IF1054.2
            020400       04 XXCORRECT                  PIC X(20).                   IF1054.2
            020500     02 INF-ANSI-REFERENCE           PIC X(48).                   IF1054.2
            020600 01  HYPHEN-LINE.                                                 IF1054.2
            020700     02 FILLER  PIC IS X VALUE IS SPACE.                          IF1054.2
            020800     02 FILLER  PIC IS X(65)    VALUE IS "************************IF1054.2
            020900-    "*****************************************".                 IF1054.2
            021000     02 FILLER  PIC IS X(54)    VALUE IS "************************IF1054.2
            021100-    "******************************".                            IF1054.2
            021200 01  CCVS-PGM-ID                     PIC X(9)   VALUE             IF1054.2
            021300     "IF105A".                                                    IF1054.2
            021400 PROCEDURE DIVISION.                                              IF1054.2
            021500 CCVS1 SECTION.                                                   IF1054.2
            021600 OPEN-FILES.                                                      IF1054.2
            021700     OPEN     OUTPUT PRINT-FILE.                                  IF1054.2
            021800     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   IF1054.2
            021900     MOVE    SPACE TO TEST-RESULTS.                               IF1054.2
            022000     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             IF1054.2
            022100     GO TO CCVS1-EXIT.                                            IF1054.2
            022200 CLOSE-FILES.                                                     IF1054.2
            022300     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   IF1054.2
            022400 TERMINATE-CCVS.                                                  IF1054.2
            022500     STOP     RUN.                                                IF1054.2
            022600 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         IF1054.2
            022700 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           IF1054.2
            022800 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          IF1054.2
            022900 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      IF1054.2
            023000     MOVE "****TEST DELETED****" TO RE-MARK.                      IF1054.2
            023100 PRINT-DETAIL.                                                    IF1054.2
            023200     IF REC-CT NOT EQUAL TO ZERO                                  IF1054.2
            023300             MOVE "." TO PARDOT-X                                 IF1054.2
            023400             MOVE REC-CT TO DOTVALUE.                             IF1054.2
            023500     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      IF1054.2
            023600     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               IF1054.2
            023700        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 IF1054.2
            023800          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 IF1054.2
            023900     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              IF1054.2
            024000     MOVE SPACE TO CORRECT-X.                                     IF1054.2
            024100     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         IF1054.2
            024200     MOVE     SPACE TO RE-MARK.                                   IF1054.2
            024300 HEAD-ROUTINE.                                                    IF1054.2
            024400     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  IF1054.2
            024500     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  IF1054.2
            024600     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  IF1054.2
            024700     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  IF1054.2
            024800 COLUMN-NAMES-ROUTINE.                                            IF1054.2
            024900     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           IF1054.2
            025000     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1054.2
            025100     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        IF1054.2
            025200 END-ROUTINE.                                                     IF1054.2
            025300     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.IF1054.2
            025400 END-RTN-EXIT.                                                    IF1054.2
            025500     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1054.2
            025600 END-ROUTINE-1.                                                   IF1054.2
            025700      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      IF1054.2
            025800      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               IF1054.2
            025900      ADD PASS-COUNTER TO ERROR-HOLD.                             IF1054.2
            026000      MOVE PASS-COUNTER TO CCVS-E-4-1.                            IF1054.2
            026100      MOVE ERROR-HOLD TO CCVS-E-4-2.                              IF1054.2
            026200      MOVE CCVS-E-4 TO CCVS-E-2-2.                                IF1054.2
            026300      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           IF1054.2
            026400  END-ROUTINE-12.                                                 IF1054.2
            026500      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        IF1054.2
            026600     IF       ERROR-COUNTER IS EQUAL TO ZERO                      IF1054.2
            026700         MOVE "NO " TO ERROR-TOTAL                                IF1054.2
            026800         ELSE                                                     IF1054.2
            026900         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       IF1054.2
            027000     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           IF1054.2
            027100     PERFORM WRITE-LINE.                                          IF1054.2
            027200 END-ROUTINE-13.                                                  IF1054.2
            027300     IF DELETE-COUNTER IS EQUAL TO ZERO                           IF1054.2
            027400         MOVE "NO " TO ERROR-TOTAL  ELSE                          IF1054.2
            027500         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      IF1054.2
            027600     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   IF1054.2
            027700     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           IF1054.2
            027800      IF   INSPECT-COUNTER EQUAL TO ZERO                          IF1054.2
            027900          MOVE "NO " TO ERROR-TOTAL                               IF1054.2
            028000      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   IF1054.2
            028100      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            IF1054.2
            028200      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          IF1054.2
            028300     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           IF1054.2
            028400 WRITE-LINE.                                                      IF1054.2
            028500     ADD 1 TO RECORD-COUNT.                                       IF1054.2
            028600*    IF RECORD-COUNT GREATER 42                                   IF1054.2
            028700*        MOVE DUMMY-RECORD TO DUMMY-HOLD                          IF1054.2
            028800*        MOVE SPACE TO DUMMY-RECORD                               IF1054.2
            028900*        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  IF1054.2
            029000*        MOVE CCVS-H-1  TO DUMMY-RECORD  PERFORM WRT-LN 2 TIMES   IF1054.2
            029100*        MOVE CCVS-H-2A TO DUMMY-RECORD  PERFORM WRT-LN 2 TIMES   IF1054.2
            029200*        MOVE CCVS-H-2B TO DUMMY-RECORD  PERFORM WRT-LN 3 TIMES   IF1054.2
            029300*        MOVE CCVS-H-3  TO DUMMY-RECORD  PERFORM WRT-LN 3 TIMES   IF1054.2
            029400*        MOVE CCVS-C-1  TO DUMMY-RECORD  PERFORM WRT-LN           IF1054.2
            029500*        MOVE CCVS-C-2  TO DUMMY-RECORD  PERFORM WRT-LN           IF1054.2
            029600*        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          IF1054.2
            029700*        MOVE DUMMY-HOLD TO DUMMY-RECORD                          IF1054.2
            029800*        MOVE ZERO TO RECORD-COUNT.                               IF1054.2
            029900     PERFORM WRT-LN.                                              IF1054.2
            030000 WRT-LN.                                                          IF1054.2
            030100     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               IF1054.2
            030200     MOVE SPACE TO DUMMY-RECORD.                                  IF1054.2
            030300 BLANK-LINE-PRINT.                                                IF1054.2
            030400     PERFORM WRT-LN.                                              IF1054.2
            030500 FAIL-ROUTINE.                                                    IF1054.2
            030600     IF     COMPUTED-X NOT EQUAL TO SPACE                         IF1054.2
            030700            GO TO FAIL-ROUTINE-WRITE.                             IF1054.2
            030800     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.IF1054.2
            030900     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 IF1054.2
            031000     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   IF1054.2
            031100     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1054.2
            031200     MOVE   SPACES TO INF-ANSI-REFERENCE.                         IF1054.2
            031300     GO TO  FAIL-ROUTINE-EX.                                      IF1054.2
            031400 FAIL-ROUTINE-WRITE.                                              IF1054.2
            031500     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE.        IF1054.2
            031600     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE                  IF1054.2
            031700                              CORMA-ANSI-REFERENCE.               IF1054.2
            031800     IF CORRECT-MIN NOT EQUAL TO SPACES THEN                      IF1054.2
            031900           MOVE TEST-CORRECT-MIN TO PRINT-REC PERFORM WRITE-LINE  IF1054.2
            032000           MOVE TEST-CORRECT-MAX TO PRINT-REC PERFORM WRITE-LINE  IF1054.2
            032100     ELSE                                                         IF1054.2
            032200           MOVE TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE.     IF1054.2
            032300     PERFORM WRITE-LINE.                                          IF1054.2
            032400     MOVE   SPACES TO COR-ANSI-REFERENCE.                         IF1054.2
            032500 FAIL-ROUTINE-EX. EXIT.                                           IF1054.2
            032600 BAIL-OUT.                                                        IF1054.2
            032700     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   IF1054.2
            032800     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           IF1054.2
            032900 BAIL-OUT-WRITE.                                                  IF1054.2
            033000     MOVE CORRECT-A TO XXCORRECT.                                 IF1054.2
            033100     MOVE COMPUTED-A TO XXCOMPUTED.                               IF1054.2
            033200     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 IF1054.2
            033300     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1054.2
            033400     MOVE   SPACES TO INF-ANSI-REFERENCE.                         IF1054.2
            033500 BAIL-OUT-EX. EXIT.                                               IF1054.2
            033600 CCVS1-EXIT.                                                      IF1054.2
            033700     EXIT.                                                        IF1054.2
            033800********************************************************          IF1054.2
            033900*                                                      *          IF1054.2
            034000*    Intrinsic Function Tests         IF105A - CHAR    *          IF1054.2
            034100*                                                      *          IF1054.2
            034200********************************************************          IF1054.2
            034300 SECT-IF105A SECTION.                                             IF1054.2
            034400 F-CHAR-INFO.                                                     IF1054.2
            034500     MOVE     "See ref. A-37 2.9" TO ANSI-REFERENCE.              IF1054.2
            034600     MOVE     "CHAR Function" TO FEATURE.                         IF1054.2
            034700*****************TEST (a) ******************************          IF1054.2
            034800 F-CHAR-01.                                                       IF1054.2
            034900     MOVE SPACE TO WS-ANUM.                                       IF1054.2
            035000 F-CHAR-TEST-01.                                                  IF1054.2
            035100     MOVE FUNCTION CHAR(37) TO WS-ANUM.                           IF1054.2
            035200     IF WS-ANUM = "${'$'}" THEN                                        IF1054.2
            035300                        PERFORM PASS                              IF1054.2
            035400     ELSE                                                         IF1054.2
            035500                        MOVE  "${'$'}"  TO CORRECT-X                   IF1054.2
            035600                        MOVE WS-ANUM TO COMPUTED-A                IF1054.2
            035700                        PERFORM FAIL.                             IF1054.2
            035800     GO TO F-CHAR-WRITE-01.                                       IF1054.2
            035900 F-CHAR-DELETE-01.                                                IF1054.2
            036000     PERFORM  DE-LETE.                                            IF1054.2
            036100     GO TO    F-CHAR-WRITE-01.                                    IF1054.2
            036200 F-CHAR-WRITE-01.                                                 IF1054.2
            036300     MOVE "F-CHAR-01" TO PAR-NAME.                                IF1054.2
            036400     PERFORM  PRINT-DETAIL.                                       IF1054.2
            036500*****************TEST (b) ******************************          IF1054.2
            036600 F-CHAR-TEST-02.                                                  IF1054.2
            036700     IF FUNCTION CHAR(B) = "${'$'}" THEN                               IF1054.2
            036800                                 PERFORM PASS                     IF1054.2
            036900     ELSE                                                         IF1054.2
            037000                                 PERFORM FAIL.                    IF1054.2
            037100     GO TO F-CHAR-WRITE-02.                                       IF1054.2
            037200 F-CHAR-DELETE-02.                                                IF1054.2
            037300     PERFORM  DE-LETE.                                            IF1054.2
            037400     GO TO    F-CHAR-WRITE-02.                                    IF1054.2
            037500 F-CHAR-WRITE-02.                                                 IF1054.2
            037600     MOVE "F-CHAR-02" TO PAR-NAME.                                IF1054.2
            037700     PERFORM  PRINT-DETAIL.                                       IF1054.2
            037800*****************TEST (c) ******************************          IF1054.2
            037900 F-CHAR-03.                                                       IF1054.2
            038000     MOVE SPACE TO WS-ANUM.                                       IF1054.2
            038100 F-CHAR-TEST-03.                                                  IF1054.2
            038200     MOVE FUNCTION CHAR(IND(5)) TO WS-ANUM.                       IF1054.2
            038300     IF WS-ANUM = "+" THEN                                        IF1054.2
            038400                        PERFORM PASS                              IF1054.2
            038500     ELSE                                                         IF1054.2
            038600                        MOVE  "+"  TO CORRECT-X                   IF1054.2
            038700                        MOVE WS-ANUM TO COMPUTED-A                IF1054.2
            038800                        PERFORM FAIL.                             IF1054.2
            038900     GO TO F-CHAR-WRITE-03.                                       IF1054.2
            039000 F-CHAR-DELETE-03.                                                IF1054.2
            039100     PERFORM  DE-LETE.                                            IF1054.2
            039200     GO TO    F-CHAR-WRITE-03.                                    IF1054.2
            039300 F-CHAR-WRITE-03.                                                 IF1054.2
            039400     MOVE "F-CHAR-03" TO PAR-NAME.                                IF1054.2
            039500     PERFORM  PRINT-DETAIL.                                       IF1054.2
            039600*****************TEST (d) ******************************          IF1054.2
            039700 F-CHAR-04.                                                       IF1054.2
            039800     MOVE SPACE TO WS-ANUM.                                       IF1054.2
            039900 F-CHAR-TEST-04.                                                  IF1054.2
            040000     MOVE FUNCTION CHAR(IND(C)) TO WS-ANUM.                       IF1054.2
            040100     IF WS-ANUM = "${'$'}" THEN                                        IF1054.2
            040200                        PERFORM PASS                              IF1054.2
            040300     ELSE                                                         IF1054.2
            040400                        MOVE  "${'$'}"  TO CORRECT-X                   IF1054.2
            040500                        MOVE WS-ANUM TO COMPUTED-A                IF1054.2
            040600                        PERFORM FAIL.                             IF1054.2
            040700     GO TO F-CHAR-WRITE-04.                                       IF1054.2
            040800 F-CHAR-DELETE-04.                                                IF1054.2
            040900     PERFORM  DE-LETE.                                            IF1054.2
            041000     GO TO    F-CHAR-WRITE-04.                                    IF1054.2
            041100 F-CHAR-WRITE-04.                                                 IF1054.2
            041200     MOVE "F-CHAR-04" TO PAR-NAME.                                IF1054.2
            041300     PERFORM  PRINT-DETAIL.                                       IF1054.2
            041400*****************TEST (e) ******************************          IF1054.2
            041500 F-CHAR-05.                                                       IF1054.2
            041600     MOVE SPACE TO WS-ANUM.                                       IF1054.2
            041700 F-CHAR-TEST-05.                                                  IF1054.2
            041800     MOVE FUNCTION CHAR(87) TO WS-ANUM.                           IF1054.2
            041900     IF WS-ANUM = "V" THEN                                        IF1054.2
            042000                        PERFORM PASS                              IF1054.2
            042100     ELSE                                                         IF1054.2
            042200                        MOVE  "V"  TO CORRECT-X                   IF1054.2
            042300                        MOVE WS-ANUM TO COMPUTED-A                IF1054.2
            042400                        PERFORM FAIL.                             IF1054.2
            042500     GO TO F-CHAR-WRITE-05.                                       IF1054.2
            042600 F-CHAR-DELETE-05.                                                IF1054.2
            042700     PERFORM  DE-LETE.                                            IF1054.2
            042800     GO TO    F-CHAR-WRITE-05.                                    IF1054.2
            042900 F-CHAR-WRITE-05.                                                 IF1054.2
            043000     MOVE "F-CHAR-05" TO PAR-NAME.                                IF1054.2
            043100     PERFORM  PRINT-DETAIL.                                       IF1054.2
            043200*****************TEST (f) ******************************          IF1054.2
            043300 F-CHAR-06.                                                       IF1054.2
            043400     MOVE SPACE TO WS-ANUM.                                       IF1054.2
            043500 F-CHAR-TEST-06.                                                  IF1054.2
            043600     MOVE FUNCTION CHAR(D) TO WS-ANUM.                            IF1054.2
            043700     IF WS-ANUM = "c" THEN                                        IF1054.2
            043800                        PERFORM PASS                              IF1054.2
            043900     ELSE                                                         IF1054.2
            044000                        MOVE  "c"  TO CORRECT-X                   IF1054.2
            044100                        MOVE WS-ANUM TO COMPUTED-A                IF1054.2
            044200                        PERFORM FAIL.                             IF1054.2
            044300     GO TO F-CHAR-WRITE-06.                                       IF1054.2
            044400 F-CHAR-DELETE-06.                                                IF1054.2
            044500     PERFORM  DE-LETE.                                            IF1054.2
            044600     GO TO    F-CHAR-WRITE-06.                                    IF1054.2
            044700 F-CHAR-WRITE-06.                                                 IF1054.2
            044800     MOVE "F-CHAR-06" TO PAR-NAME.                                IF1054.2
            044900     PERFORM  PRINT-DETAIL.                                       IF1054.2
            045000*****************TEST (g) ******************************          IF1054.2
            045100 F-CHAR-07.                                                       IF1054.2
            045200     MOVE SPACE TO WS-ANUM.                                       IF1054.2
            045300 F-CHAR-TEST-07.                                                  IF1054.2
            045400                                                                  IF1054.2
            045500     IF FUNCTION ORD(FUNCTION CHAR(2)) = 2 THEN                   IF1054.2
            045600                        PERFORM PASS                              IF1054.2
            045700     ELSE                                                         IF1054.2
            045800                        MOVE  2  TO CORRECT-N                     IF1054.2
            045900                        MOVE WS-ANUM TO COMPUTED-A                IF1054.2
            046000                        PERFORM FAIL.                             IF1054.2
            046100     GO TO F-CHAR-WRITE-07.                                       IF1054.2
            046200 F-CHAR-DELETE-07.                                                IF1054.2
            046300     PERFORM  DE-LETE.                                            IF1054.2
            046400     GO TO    F-CHAR-WRITE-07.                                    IF1054.2
            046500 F-CHAR-WRITE-07.                                                 IF1054.2
            046600     MOVE "F-CHAR-07" TO PAR-NAME.                                IF1054.2
            046700     PERFORM  PRINT-DETAIL.                                       IF1054.2
            046800*****************TEST (h) ******************************          IF1054.2
            046900 F-CHAR-08.                                                       IF1054.2
            047000     MOVE SPACE TO WS-ANUM.                                       IF1054.2
            047100 F-CHAR-TEST-08.                                                  IF1054.2
            047200     IF FUNCTION ORD(FUNCTION CHAR(4)) +                          IF1054.2
            047300        FUNCTION ORD(FUNCTION CHAR(7)) = 11 THEN                  IF1054.2
            047400                        PERFORM PASS                              IF1054.2
            047500     ELSE                                                         IF1054.2
            047600                        PERFORM FAIL.                             IF1054.2
            047700     GO TO F-CHAR-WRITE-08.                                       IF1054.2
            047800 F-CHAR-DELETE-08.                                                IF1054.2
            047900     PERFORM  DE-LETE.                                            IF1054.2
            048000     GO TO    F-CHAR-WRITE-08.                                    IF1054.2
            048100 F-CHAR-WRITE-08.                                                 IF1054.2
            048200     MOVE "F-CHAR-08" TO PAR-NAME.                                IF1054.2
            048300     PERFORM  PRINT-DETAIL.                                       IF1054.2
            048400*******************END OF TESTS**************************         IF1054.2
            048500 CCVS-EXIT SECTION.                                               IF1054.2
            048600 CCVS-999999.                                                     IF1054.2
            048700     GO TO CLOSE-FILES.                                           IF1054.2
                  *END-OF,IF105A                                                                  
        """)
    )

    @Test
    fun if1064_2() = rewriteRun(
        cobol("""
                  *HEADER,COBOL,IF106A                                                            
            000100 IDENTIFICATION DIVISION.                                         IF1064.2
            000200 PROGRAM-ID.                                                      IF1064.2
            000300     IF106A.                                                      IF1064.2
            000400                                                                  IF1064.2
            000500***********************************************************       IF1064.2
            000600*                                                         *       IF1064.2
            000700* This program forms part of the CCVS85 COBOL Test Suite. *       IF1064.2
            000800* It contains tests for the Intrinsic Function COS.       *       IF1064.2
            000900*                                                         *       IF1064.2
            001000***********************************************************       IF1064.2
            001100 ENVIRONMENT DIVISION.                                            IF1064.2
            001200 CONFIGURATION SECTION.                                           IF1064.2
            001300 SOURCE-COMPUTER.                                                 IF1064.2
            001400     XXXXX082.                                                    IF1064.2
            001500 OBJECT-COMPUTER.                                                 IF1064.2
            001600     XXXXX083.                                                    IF1064.2
            001700 INPUT-OUTPUT SECTION.                                            IF1064.2
            001800 FILE-CONTROL.                                                    IF1064.2
            001900     SELECT PRINT-FILE ASSIGN TO                                  IF1064.2
            002000     XXXXX055.                                                    IF1064.2
            002100 DATA DIVISION.                                                   IF1064.2
            002200 FILE SECTION.                                                    IF1064.2
            002300 FD  PRINT-FILE.                                                  IF1064.2
            002400 01  PRINT-REC PICTURE X(120).                                    IF1064.2
            002500 01  DUMMY-RECORD PICTURE X(120).                                 IF1064.2
            002600 WORKING-STORAGE SECTION.                                         IF1064.2
            002700***********************************************************       IF1064.2
            002800* Variables specific to the Intrinsic Function Test IF106A*       IF1064.2
            002900***********************************************************       IF1064.2
            003000 01  A                   PIC S9(5)V9(5)      VALUE -0.00004.      IF1064.2
            003100 01  B                   PIC S9(5)V9(5)      VALUE 14000.105.     IF1064.2
            003200 01  C                   PIC S9(10)          VALUE 100000.        IF1064.2
            003300 01  D                   PIC S9(10)          VALUE 1000.          IF1064.2
            003400 01  E                   PIC S9(10)          VALUE 3.             IF1064.2
            003500 01  PI                  PIC S9V9(17)        VALUE 3.141592654.   IF1064.2
            003600 01  MINUSPI             PIC S9V9(17)        VALUE -3.141592654.  IF1064.2
            003700 01  ARG1                PIC S9V9(17)        VALUE 1.00.          IF1064.2
            003800 01  ARR                                     VALUE "40537".       IF1064.2
            003900     02  IND OCCURS 5 TIMES PIC 9.                                IF1064.2
            004000 01  TEMP                PIC S9(5)V9(5).                          IF1064.2
            004100 01  WS-NUM              PIC S9(5)V9(6).                          IF1064.2
            004200 01  MIN-RANGE           PIC S9(5)V9(7).                          IF1064.2
            004300 01  MAX-RANGE           PIC S9(5)V9(7).                          IF1064.2
            004400*                                                                 IF1064.2
            004500**********************************************************        IF1064.2
            004600*                                                                 IF1064.2
            004700 01  TEST-RESULTS.                                                IF1064.2
            004800     02 FILLER                   PIC X      VALUE SPACE.          IF1064.2
            004900     02 FEATURE                  PIC X(20)  VALUE SPACE.          IF1064.2
            005000     02 FILLER                   PIC X      VALUE SPACE.          IF1064.2
            005100     02 P-OR-F                   PIC X(5)   VALUE SPACE.          IF1064.2
            005200     02 FILLER                   PIC X      VALUE SPACE.          IF1064.2
            005300     02  PAR-NAME.                                                IF1064.2
            005400       03 FILLER                 PIC X(19)  VALUE SPACE.          IF1064.2
            005500       03  PARDOT-X              PIC X      VALUE SPACE.          IF1064.2
            005600       03 DOTVALUE               PIC 99     VALUE ZERO.           IF1064.2
            005700     02 FILLER                   PIC X(8)   VALUE SPACE.          IF1064.2
            005800     02 RE-MARK                  PIC X(61).                       IF1064.2
            005900 01  TEST-COMPUTED.                                               IF1064.2
            006000     02 FILLER                   PIC X(30)  VALUE SPACE.          IF1064.2
            006100     02 FILLER                   PIC X(17)  VALUE                 IF1064.2
            006200            "       COMPUTED=".                                   IF1064.2
            006300     02 COMPUTED-X.                                               IF1064.2
            006400     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          IF1064.2
            006500     03 COMPUTED-N               REDEFINES COMPUTED-A             IF1064.2
            006600                                 PIC -9(9).9(9).                  IF1064.2
            006700     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         IF1064.2
            006800     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     IF1064.2
            006900     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     IF1064.2
            007000     03       CM-18V0 REDEFINES COMPUTED-A.                       IF1064.2
            007100         04 COMPUTED-18V0                    PIC -9(18).          IF1064.2
            007200         04 FILLER                           PIC X.               IF1064.2
            007300     03 FILLER PIC X(50) VALUE SPACE.                             IF1064.2
            007400 01  TEST-CORRECT.                                                IF1064.2
            007500     02 FILLER PIC X(30) VALUE SPACE.                             IF1064.2
            007600     02 FILLER PIC X(17) VALUE "       CORRECT =".                IF1064.2
            007700     02 CORRECT-X.                                                IF1064.2
            007800     03 CORRECT-A                  PIC X(20) VALUE SPACE.         IF1064.2
            007900     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      IF1064.2
            008000     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         IF1064.2
            008100     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     IF1064.2
            008200     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     IF1064.2
            008300     03      CR-18V0 REDEFINES CORRECT-A.                         IF1064.2
            008400         04 CORRECT-18V0                     PIC -9(18).          IF1064.2
            008500         04 FILLER                           PIC X.               IF1064.2
            008600     03 FILLER PIC X(2) VALUE SPACE.                              IF1064.2
            008700     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     IF1064.2
            008800 01  TEST-CORRECT-MIN.                                            IF1064.2
            008900     02 FILLER PIC X(30) VALUE SPACE.                             IF1064.2
            009000     02 FILLER PIC X(17) VALUE "     MIN VALUE =".                IF1064.2
            009100     02 CORRECTMI-X.                                              IF1064.2
            009200     03 CORRECTMI-A                 PIC X(20) VALUE SPACE.        IF1064.2
            009300     03 CORRECT-MIN    REDEFINES CORRECTMI-A     PIC -9(9).9(9).  IF1064.2
            009400     03 CORRECTMI-0V18 REDEFINES CORRECTMI-A     PIC -.9(18).     IF1064.2
            009500     03 CORRECTMI-4V14 REDEFINES CORRECTMI-A     PIC -9(4).9(14). IF1064.2
            009600     03 CORRECTMI-14V4 REDEFINES CORRECTMI-A     PIC -9(14).9(4). IF1064.2
            009700     03      CR-18V0 REDEFINES CORRECTMI-A.                       IF1064.2
            009800         04 CORRECTMI-18V0                     PIC -9(18).        IF1064.2
            009900         04 FILLER                           PIC X.               IF1064.2
            010000     03 FILLER PIC X(2) VALUE SPACE.                              IF1064.2
            010100     03 FILLER                           PIC X(48) VALUE SPACE.   IF1064.2
            010200 01  TEST-CORRECT-MAX.                                            IF1064.2
            010300     02 FILLER PIC X(30) VALUE SPACE.                             IF1064.2
            010400     02 FILLER PIC X(17) VALUE "     MAX VALUE =".                IF1064.2
            010500     02 CORRECTMA-X.                                              IF1064.2
            010600     03 CORRECTMA-A                  PIC X(20) VALUE SPACE.       IF1064.2
            010700     03 CORRECT-MAX    REDEFINES CORRECTMA-A     PIC -9(9).9(9).  IF1064.2
            010800     03 CORRECTMA-0V18 REDEFINES CORRECTMA-A     PIC -.9(18).     IF1064.2
            010900     03 CORRECTMA-4V14 REDEFINES CORRECTMA-A     PIC -9(4).9(14). IF1064.2
            011000     03 CORRECTMA-14V4 REDEFINES CORRECTMA-A     PIC -9(14).9(4). IF1064.2
            011100     03      CR-18V0 REDEFINES CORRECTMA-A.                       IF1064.2
            011200         04 CORRECTMA-18V0                     PIC -9(18).        IF1064.2
            011300         04 FILLER                           PIC X.               IF1064.2
            011400     03 FILLER PIC X(2) VALUE SPACE.                              IF1064.2
            011500     03 CORMA-ANSI-REFERENCE             PIC X(48) VALUE SPACE.   IF1064.2
            011600 01  CCVS-C-1.                                                    IF1064.2
            011700     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PAIF1064.2
            011800-    "SS  PARAGRAPH-NAME                                          IF1064.2
            011900-    "       REMARKS".                                            IF1064.2
            012000     02 FILLER                     PIC X(20)    VALUE SPACE.      IF1064.2
            012100 01  CCVS-C-2.                                                    IF1064.2
            012200     02 FILLER                     PIC X        VALUE SPACE.      IF1064.2
            012300     02 FILLER                     PIC X(6)     VALUE "TESTED".   IF1064.2
            012400     02 FILLER                     PIC X(15)    VALUE SPACE.      IF1064.2
            012500     02 FILLER                     PIC X(4)     VALUE "FAIL".     IF1064.2
            012600     02 FILLER                     PIC X(94)    VALUE SPACE.      IF1064.2
            012700 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       IF1064.2
            012800 01  REC-CT                        PIC 99       VALUE ZERO.       IF1064.2
            012900 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       IF1064.2
            013000 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       IF1064.2
            013100 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       IF1064.2
            013200 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       IF1064.2
            013300 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       IF1064.2
            013400 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       IF1064.2
            013500 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      IF1064.2
            013600 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       IF1064.2
            013700 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     IF1064.2
            013800 01  CCVS-H-1.                                                    IF1064.2
            013900     02  FILLER                    PIC X(39)    VALUE SPACES.     IF1064.2
            014000     02  FILLER                    PIC X(42)    VALUE             IF1064.2
            014100     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 IF1064.2
            014200     02  FILLER                    PIC X(39)    VALUE SPACES.     IF1064.2
            014300 01  CCVS-H-2A.                                                   IF1064.2
            014400   02  FILLER                        PIC X(40)  VALUE SPACE.      IF1064.2
            014500   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  IF1064.2
            014600   02  FILLER                        PIC XXXX   VALUE             IF1064.2
            014700     "4.2 ".                                                      IF1064.2
            014800   02  FILLER                        PIC X(28)  VALUE             IF1064.2
            014900            " COPY - NOT FOR DISTRIBUTION".                       IF1064.2
            015000   02  FILLER                        PIC X(41)  VALUE SPACE.      IF1064.2
            015100                                                                  IF1064.2
            015200 01  CCVS-H-2B.                                                   IF1064.2
            015300   02  FILLER                        PIC X(15)  VALUE             IF1064.2
            015400            "TEST RESULT OF ".                                    IF1064.2
            015500   02  TEST-ID                       PIC X(9).                    IF1064.2
            015600   02  FILLER                        PIC X(4)   VALUE             IF1064.2
            015700            " IN ".                                               IF1064.2
            015800   02  FILLER                        PIC X(12)  VALUE             IF1064.2
            015900     " HIGH       ".                                              IF1064.2
            016000   02  FILLER                        PIC X(22)  VALUE             IF1064.2
            016100            " LEVEL VALIDATION FOR ".                             IF1064.2
            016200   02  FILLER                        PIC X(58)  VALUE             IF1064.2
            016300     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".IF1064.2
            016400 01  CCVS-H-3.                                                    IF1064.2
            016500     02  FILLER                      PIC X(34)  VALUE             IF1064.2
            016600            " FOR OFFICIAL USE ONLY    ".                         IF1064.2
            016700     02  FILLER                      PIC X(58)  VALUE             IF1064.2
            016800     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".IF1064.2
            016900     02  FILLER                      PIC X(28)  VALUE             IF1064.2
            017000            "  COPYRIGHT   1985 ".                                IF1064.2
            017100 01  CCVS-E-1.                                                    IF1064.2
            017200     02 FILLER                       PIC X(52)  VALUE SPACE.      IF1064.2
            017300     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              IF1064.2
            017400     02 ID-AGAIN                     PIC X(9).                    IF1064.2
            017500     02 FILLER                       PIC X(45)  VALUE SPACES.     IF1064.2
            017600 01  CCVS-E-2.                                                    IF1064.2
            017700     02  FILLER                      PIC X(31)  VALUE SPACE.      IF1064.2
            017800     02  FILLER                      PIC X(21)  VALUE SPACE.      IF1064.2
            017900     02 CCVS-E-2-2.                                               IF1064.2
            018000         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      IF1064.2
            018100         03 FILLER                   PIC X      VALUE SPACE.      IF1064.2
            018200         03 ENDER-DESC               PIC X(44)  VALUE             IF1064.2
            018300            "ERRORS ENCOUNTERED".                                 IF1064.2
            018400 01  CCVS-E-3.                                                    IF1064.2
            018500     02  FILLER                      PIC X(22)  VALUE             IF1064.2
            018600            " FOR OFFICIAL USE ONLY".                             IF1064.2
            018700     02  FILLER                      PIC X(12)  VALUE SPACE.      IF1064.2
            018800     02  FILLER                      PIC X(58)  VALUE             IF1064.2
            018900     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".IF1064.2
            019000     02  FILLER                      PIC X(13)  VALUE SPACE.      IF1064.2
            019100     02 FILLER                       PIC X(15)  VALUE             IF1064.2
            019200             " COPYRIGHT 1985".                                   IF1064.2
            019300 01  CCVS-E-4.                                                    IF1064.2
            019400     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      IF1064.2
            019500     02 FILLER                       PIC X(4)   VALUE " OF ".     IF1064.2
            019600     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      IF1064.2
            019700     02 FILLER                       PIC X(40)  VALUE             IF1064.2
            019800      "  TESTS WERE EXECUTED SUCCESSFULLY".                       IF1064.2
            019900 01  XXINFO.                                                      IF1064.2
            020000     02 FILLER                       PIC X(19)  VALUE             IF1064.2
            020100            "*** INFORMATION ***".                                IF1064.2
            020200     02 INFO-TEXT.                                                IF1064.2
            020300       04 FILLER                     PIC X(8)   VALUE SPACE.      IF1064.2
            020400       04 XXCOMPUTED                 PIC X(20).                   IF1064.2
            020500       04 FILLER                     PIC X(5)   VALUE SPACE.      IF1064.2
            020600       04 XXCORRECT                  PIC X(20).                   IF1064.2
            020700     02 INF-ANSI-REFERENCE           PIC X(48).                   IF1064.2
            020800 01  HYPHEN-LINE.                                                 IF1064.2
            020900     02 FILLER  PIC IS X VALUE IS SPACE.                          IF1064.2
            021000     02 FILLER  PIC IS X(65)    VALUE IS "************************IF1064.2
            021100-    "*****************************************".                 IF1064.2
            021200     02 FILLER  PIC IS X(54)    VALUE IS "************************IF1064.2
            021300-    "******************************".                            IF1064.2
            021400 01  CCVS-PGM-ID                     PIC X(9)   VALUE             IF1064.2
            021500     "IF106A".                                                    IF1064.2
            021600 PROCEDURE DIVISION.                                              IF1064.2
            021700 CCVS1 SECTION.                                                   IF1064.2
            021800 OPEN-FILES.                                                      IF1064.2
            021900     OPEN     OUTPUT PRINT-FILE.                                  IF1064.2
            022000     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   IF1064.2
            022100     MOVE    SPACE TO TEST-RESULTS.                               IF1064.2
            022200     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             IF1064.2
            022300     GO TO CCVS1-EXIT.                                            IF1064.2
            022400 CLOSE-FILES.                                                     IF1064.2
            022500     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   IF1064.2
            022600 TERMINATE-CCVS.                                                  IF1064.2
            022700     STOP     RUN.                                                IF1064.2
            022800 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         IF1064.2
            022900 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           IF1064.2
            023000 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          IF1064.2
            023100 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      IF1064.2
            023200     MOVE "****TEST DELETED****" TO RE-MARK.                      IF1064.2
            023300 PRINT-DETAIL.                                                    IF1064.2
            023400     IF REC-CT NOT EQUAL TO ZERO                                  IF1064.2
            023500             MOVE "." TO PARDOT-X                                 IF1064.2
            023600             MOVE REC-CT TO DOTVALUE.                             IF1064.2
            023700     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      IF1064.2
            023800     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               IF1064.2
            023900        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 IF1064.2
            024000          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 IF1064.2
            024100     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              IF1064.2
            024200     MOVE SPACE TO CORRECT-X.                                     IF1064.2
            024300     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         IF1064.2
            024400     MOVE     SPACE TO RE-MARK.                                   IF1064.2
            024500 HEAD-ROUTINE.                                                    IF1064.2
            024600     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  IF1064.2
            024700     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  IF1064.2
            024800     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  IF1064.2
            024900     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  IF1064.2
            025000 COLUMN-NAMES-ROUTINE.                                            IF1064.2
            025100     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           IF1064.2
            025200     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1064.2
            025300     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        IF1064.2
            025400 END-ROUTINE.                                                     IF1064.2
            025500     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.IF1064.2
            025600 END-RTN-EXIT.                                                    IF1064.2
            025700     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1064.2
            025800 END-ROUTINE-1.                                                   IF1064.2
            025900      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      IF1064.2
            026000      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               IF1064.2
            026100      ADD PASS-COUNTER TO ERROR-HOLD.                             IF1064.2
            026200      MOVE PASS-COUNTER TO CCVS-E-4-1.                            IF1064.2
            026300      MOVE ERROR-HOLD TO CCVS-E-4-2.                              IF1064.2
            026400      MOVE CCVS-E-4 TO CCVS-E-2-2.                                IF1064.2
            026500      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           IF1064.2
            026600  END-ROUTINE-12.                                                 IF1064.2
            026700      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        IF1064.2
            026800     IF       ERROR-COUNTER IS EQUAL TO ZERO                      IF1064.2
            026900         MOVE "NO " TO ERROR-TOTAL                                IF1064.2
            027000         ELSE                                                     IF1064.2
            027100         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       IF1064.2
            027200     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           IF1064.2
            027300     PERFORM WRITE-LINE.                                          IF1064.2
            027400 END-ROUTINE-13.                                                  IF1064.2
            027500     IF DELETE-COUNTER IS EQUAL TO ZERO                           IF1064.2
            027600         MOVE "NO " TO ERROR-TOTAL  ELSE                          IF1064.2
            027700         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      IF1064.2
            027800     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   IF1064.2
            027900     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           IF1064.2
            028000      IF   INSPECT-COUNTER EQUAL TO ZERO                          IF1064.2
            028100          MOVE "NO " TO ERROR-TOTAL                               IF1064.2
            028200      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   IF1064.2
            028300      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            IF1064.2
            028400      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          IF1064.2
            028500     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           IF1064.2
            028600 WRITE-LINE.                                                      IF1064.2
            028700     ADD 1 TO RECORD-COUNT.                                       IF1064.2
            028800*    IF RECORD-COUNT GREATER 42                                   IF1064.2
            028900*        MOVE DUMMY-RECORD TO DUMMY-HOLD                          IF1064.2
            029000*        MOVE SPACE TO DUMMY-RECORD                               IF1064.2
            029100*        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  IF1064.2
            029200*        MOVE CCVS-H-1  TO DUMMY-RECORD  PERFORM WRT-LN 2 TIMES   IF1064.2
            029300*        MOVE CCVS-H-2A TO DUMMY-RECORD  PERFORM WRT-LN 2 TIMES   IF1064.2
            029400*        MOVE CCVS-H-2B TO DUMMY-RECORD  PERFORM WRT-LN 3 TIMES   IF1064.2
            029500*        MOVE CCVS-H-3  TO DUMMY-RECORD  PERFORM WRT-LN 3 TIMES   IF1064.2
            029600*        MOVE CCVS-C-1  TO DUMMY-RECORD  PERFORM WRT-LN           IF1064.2
            029700*        MOVE CCVS-C-2  TO DUMMY-RECORD  PERFORM WRT-LN           IF1064.2
            029800*        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          IF1064.2
            029900*        MOVE DUMMY-HOLD TO DUMMY-RECORD                          IF1064.2
            030000*        MOVE ZERO TO RECORD-COUNT.                               IF1064.2
            030100     PERFORM WRT-LN.                                              IF1064.2
            030200 WRT-LN.                                                          IF1064.2
            030300     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               IF1064.2
            030400     MOVE SPACE TO DUMMY-RECORD.                                  IF1064.2
            030500 BLANK-LINE-PRINT.                                                IF1064.2
            030600     PERFORM WRT-LN.                                              IF1064.2
            030700 FAIL-ROUTINE.                                                    IF1064.2
            030800     IF     COMPUTED-X NOT EQUAL TO SPACE                         IF1064.2
            030900            GO TO FAIL-ROUTINE-WRITE.                             IF1064.2
            031000     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.IF1064.2
            031100     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 IF1064.2
            031200     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   IF1064.2
            031300     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1064.2
            031400     MOVE   SPACES TO INF-ANSI-REFERENCE.                         IF1064.2
            031500     GO TO  FAIL-ROUTINE-EX.                                      IF1064.2
            031600 FAIL-ROUTINE-WRITE.                                              IF1064.2
            031700     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE.        IF1064.2
            031800     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE                  IF1064.2
            031900                              CORMA-ANSI-REFERENCE.               IF1064.2
            032000     IF CORRECT-MIN NOT EQUAL TO SPACES THEN                      IF1064.2
            032100           MOVE TEST-CORRECT-MIN TO PRINT-REC PERFORM WRITE-LINE  IF1064.2
            032200           MOVE TEST-CORRECT-MAX TO PRINT-REC PERFORM WRITE-LINE  IF1064.2
            032300     ELSE                                                         IF1064.2
            032400           MOVE TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE.     IF1064.2
            032500     PERFORM WRITE-LINE.                                          IF1064.2
            032600     MOVE   SPACES TO COR-ANSI-REFERENCE.                         IF1064.2
            032700 FAIL-ROUTINE-EX. EXIT.                                           IF1064.2
            032800 BAIL-OUT.                                                        IF1064.2
            032900     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   IF1064.2
            033000     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           IF1064.2
            033100 BAIL-OUT-WRITE.                                                  IF1064.2
            033200     MOVE CORRECT-A TO XXCORRECT.                                 IF1064.2
            033300     MOVE COMPUTED-A TO XXCOMPUTED.                               IF1064.2
            033400     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 IF1064.2
            033500     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1064.2
            033600     MOVE   SPACES TO INF-ANSI-REFERENCE.                         IF1064.2
            033700 BAIL-OUT-EX. EXIT.                                               IF1064.2
            033800 CCVS1-EXIT.                                                      IF1064.2
            033900     EXIT.                                                        IF1064.2
            034000********************************************************          IF1064.2
            034100*                                                      *          IF1064.2
            034200*    Intrinsic Function Tests         IF106A - COS     *          IF1064.2
            034300*                                                      *          IF1064.2
            034400********************************************************          IF1064.2
            034500 SECT-IF106A SECTION.                                             IF1064.2
            034600 F-COS-INFO.                                                      IF1064.2
            034700     MOVE     "See ref. A-38 2.8" TO ANSI-REFERENCE.              IF1064.2
            034800     MOVE     "COS Function" TO FEATURE.                          IF1064.2
            034900*****************TEST (a) - SIMPLE TEST*****************          IF1064.2
            035000 F-COS-01.                                                        IF1064.2
            035100     MOVE ZERO TO WS-NUM.                                         IF1064.2
            035200     MOVE  0.999980 TO MIN-RANGE.                                 IF1064.2
            035300     MOVE  1.00000 TO MAX-RANGE.                                  IF1064.2
            035400 F-COS-TEST-01.                                                   IF1064.2
            035500     COMPUTE WS-NUM = FUNCTION COS(0).                            IF1064.2
            035600     IF (WS-NUM >= MIN-RANGE) AND                                 IF1064.2
            035700        (WS-NUM <= MAX-RANGE) THEN                                IF1064.2
            035800                    PERFORM PASS                                  IF1064.2
            035900     ELSE                                                         IF1064.2
            036000                    MOVE WS-NUM TO COMPUTED-N                     IF1064.2
            036100                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1064.2
            036200                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1064.2
            036300                    PERFORM FAIL.                                 IF1064.2
            036400     GO TO F-COS-WRITE-01.                                        IF1064.2
            036500 F-COS-DELETE-01.                                                 IF1064.2
            036600     PERFORM  DE-LETE.                                            IF1064.2
            036700     GO TO    F-COS-WRITE-01.                                     IF1064.2
            036800 F-COS-WRITE-01.                                                  IF1064.2
            036900     MOVE "F-COS-01" TO PAR-NAME.                                 IF1064.2
            037000     PERFORM  PRINT-DETAIL.                                       IF1064.2
            037100*****************TEST (b) - SIMPLE TEST*****************          IF1064.2
            037200 F-COS-02.                                                        IF1064.2
            037300     MOVE ZERO TO WS-NUM.                                         IF1064.2
            037400     MOVE -1.00000   TO MIN-RANGE.                                IF1064.2
            037500     MOVE -0.999980 TO MAX-RANGE.                                 IF1064.2
            037600 F-COS-TEST-02.                                                   IF1064.2
            037700     COMPUTE WS-NUM = FUNCTION COS(PI).                           IF1064.2
            037800     IF (WS-NUM >= MIN-RANGE) AND                                 IF1064.2
            037900        (WS-NUM <= MAX-RANGE) THEN                                IF1064.2
            038000                    PERFORM PASS                                  IF1064.2
            038100     ELSE                                                         IF1064.2
            038200                    MOVE WS-NUM TO COMPUTED-N                     IF1064.2
            038300                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1064.2
            038400                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1064.2
            038500                    PERFORM FAIL.                                 IF1064.2
            038600     GO TO F-COS-WRITE-02.                                        IF1064.2
            038700 F-COS-DELETE-02.                                                 IF1064.2
            038800     PERFORM  DE-LETE.                                            IF1064.2
            038900     GO TO    F-COS-WRITE-02.                                     IF1064.2
            039000 F-COS-WRITE-02.                                                  IF1064.2
            039100     MOVE "F-COS-02" TO PAR-NAME.                                 IF1064.2
            039200     PERFORM  PRINT-DETAIL.                                       IF1064.2
            039300*****************TEST (c) - SIMPLE TEST*****************          IF1064.2
            039400 F-COS-03.                                                        IF1064.2
            039500     MOVE ZERO TO WS-NUM.                                         IF1064.2
            039600     MOVE -1.00000 TO MIN-RANGE.                                  IF1064.2
            039700     MOVE -0.999980 TO MAX-RANGE.                                 IF1064.2
            039800 F-COS-TEST-03.                                                   IF1064.2
            039900     COMPUTE WS-NUM = FUNCTION COS(MINUSPI).                      IF1064.2
            040000     IF (WS-NUM >= MIN-RANGE) AND                                 IF1064.2
            040100        (WS-NUM <= MAX-RANGE) THEN                                IF1064.2
            040200                    PERFORM PASS                                  IF1064.2
            040300     ELSE                                                         IF1064.2
            040400                    MOVE WS-NUM TO COMPUTED-N                     IF1064.2
            040500                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1064.2
            040600                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1064.2
            040700                    PERFORM FAIL.                                 IF1064.2
            040800     GO TO F-COS-WRITE-03.                                        IF1064.2
            040900 F-COS-DELETE-03.                                                 IF1064.2
            041000     PERFORM  DE-LETE.                                            IF1064.2
            041100     GO TO    F-COS-WRITE-03.                                     IF1064.2
            041200 F-COS-WRITE-03.                                                  IF1064.2
            041300     MOVE "F-COS-03" TO PAR-NAME.                                 IF1064.2
            041400     PERFORM  PRINT-DETAIL.                                       IF1064.2
            041500*****************TEST (d) - SIMPLE TEST*****************          IF1064.2
            041600 F-COS-04.                                                        IF1064.2
            041700     MOVE ZERO TO WS-NUM.                                         IF1064.2
            041800     MOVE  0.999980 TO MIN-RANGE.                                 IF1064.2
            041900     MOVE  1.000000 TO MAX-RANGE.                                 IF1064.2
            042000 F-COS-TEST-04.                                                   IF1064.2
            042100     COMPUTE WS-NUM = FUNCTION COS(0.001).                        IF1064.2
            042200     IF (WS-NUM >= MIN-RANGE) AND                                 IF1064.2
            042300        (WS-NUM <= MAX-RANGE) THEN                                IF1064.2
            042400                    PERFORM PASS                                  IF1064.2
            042500     ELSE                                                         IF1064.2
            042600                    MOVE WS-NUM TO COMPUTED-N                     IF1064.2
            042700                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1064.2
            042800                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1064.2
            042900                    PERFORM FAIL.                                 IF1064.2
            043000     GO TO F-COS-WRITE-04.                                        IF1064.2
            043100 F-COS-DELETE-04.                                                 IF1064.2
            043200     PERFORM  DE-LETE.                                            IF1064.2
            043300     GO TO    F-COS-WRITE-04.                                     IF1064.2
            043400 F-COS-WRITE-04.                                                  IF1064.2
            043500     MOVE "F-COS-04" TO PAR-NAME.                                 IF1064.2
            043600     PERFORM  PRINT-DETAIL.                                       IF1064.2
            043700*****************TEST (e) - SIMPLE TEST*****************          IF1064.2
            043800 F-COS-05.                                                        IF1064.2
            043900     MOVE ZERO TO WS-NUM.                                         IF1064.2
            044000     MOVE  0.999980 TO MIN-RANGE.                                 IF1064.2
            044100     MOVE  1.000000 TO MAX-RANGE.                                 IF1064.2
            044200 F-COS-TEST-05.                                                   IF1064.2
            044300     COMPUTE WS-NUM = FUNCTION COS(.00009).                       IF1064.2
            044400     IF (WS-NUM >= MIN-RANGE) AND                                 IF1064.2
            044500        (WS-NUM <= MAX-RANGE) THEN                                IF1064.2
            044600                    PERFORM PASS                                  IF1064.2
            044700     ELSE                                                         IF1064.2
            044800                    MOVE WS-NUM TO COMPUTED-N                     IF1064.2
            044900                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1064.2
            045000                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1064.2
            045100                    PERFORM FAIL.                                 IF1064.2
            045200     GO TO F-COS-WRITE-05.                                        IF1064.2
            045300 F-COS-DELETE-05.                                                 IF1064.2
            045400     PERFORM  DE-LETE.                                            IF1064.2
            045500     GO TO    F-COS-WRITE-05.                                     IF1064.2
            045600 F-COS-WRITE-05.                                                  IF1064.2
            045700     MOVE "F-COS-05" TO PAR-NAME.                                 IF1064.2
            045800     PERFORM  PRINT-DETAIL.                                       IF1064.2
            045900*****************TEST (f) - SIMPLE TEST*****************          IF1064.2
            046000 F-COS-06.                                                        IF1064.2
            046100     MOVE ZERO TO WS-NUM.                                         IF1064.2
            046200     MOVE  0.99998  TO MIN-RANGE.                                 IF1064.2
            046300     MOVE  1.000000 TO MAX-RANGE.                                 IF1064.2
            046400 F-COS-TEST-06.                                                   IF1064.2
            046500     COMPUTE WS-NUM = FUNCTION COS(A).                            IF1064.2
            046600     IF (WS-NUM >= MIN-RANGE) AND                                 IF1064.2
            046700        (WS-NUM <= MAX-RANGE) THEN                                IF1064.2
            046800                    PERFORM PASS                                  IF1064.2
            046900     ELSE                                                         IF1064.2
            047000                    MOVE WS-NUM TO COMPUTED-N                     IF1064.2
            047100                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1064.2
            047200                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1064.2
            047300                    PERFORM FAIL.                                 IF1064.2
            047400     GO TO F-COS-WRITE-06.                                        IF1064.2
            047500 F-COS-DELETE-06.                                                 IF1064.2
            047600     PERFORM  DE-LETE.                                            IF1064.2
            047700     GO TO    F-COS-WRITE-06.                                     IF1064.2
            047800 F-COS-WRITE-06.                                                  IF1064.2
            047900     MOVE "F-COS-06" TO PAR-NAME.                                 IF1064.2
            048000     PERFORM  PRINT-DETAIL.                                       IF1064.2
            048100*****************TEST (g) - SIMPLE TEST*****************          IF1064.2
            048200 F-COS-07.                                                        IF1064.2
            048300     MOVE ZERO TO WS-NUM.                                         IF1064.2
            048400     MOVE  0.283656 TO MIN-RANGE.                                 IF1064.2
            048500     MOVE  0.283668 TO MAX-RANGE.                                 IF1064.2
            048600 F-COS-TEST-07.                                                   IF1064.2
            048700     COMPUTE WS-NUM = FUNCTION COS(IND(E)).                       IF1064.2
            048800     IF (WS-NUM >= MIN-RANGE) AND                                 IF1064.2
            048900        (WS-NUM <= MAX-RANGE) THEN                                IF1064.2
            049000                    PERFORM PASS                                  IF1064.2
            049100     ELSE                                                         IF1064.2
            049200                    MOVE WS-NUM TO COMPUTED-N                     IF1064.2
            049300                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1064.2
            049400                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1064.2
            049500                    PERFORM FAIL.                                 IF1064.2
            049600     GO TO F-COS-WRITE-07.                                        IF1064.2
            049700 F-COS-DELETE-07.                                                 IF1064.2
            049800     PERFORM  DE-LETE.                                            IF1064.2
            049900     GO TO    F-COS-WRITE-07.                                     IF1064.2
            050000 F-COS-WRITE-07.                                                  IF1064.2
            050100     MOVE "F-COS-07" TO PAR-NAME.                                 IF1064.2
            050200     PERFORM  PRINT-DETAIL.                                       IF1064.2
            050300*****************TEST (h) - SIMPLE TEST*****************          IF1064.2
            050400 F-COS-08.                                                        IF1064.2
            050500     MOVE ZERO TO WS-NUM.                                         IF1064.2
            050600     MOVE  0.753887 TO MIN-RANGE.                                 IF1064.2
            050700     MOVE  0.753917 TO MAX-RANGE.                                 IF1064.2
            050800 F-COS-TEST-08.                                                   IF1064.2
            050900     COMPUTE WS-NUM = FUNCTION COS(IND(5)).                       IF1064.2
            051000     IF (WS-NUM >= MIN-RANGE) AND                                 IF1064.2
            051100        (WS-NUM <= MAX-RANGE) THEN                                IF1064.2
            051200                    PERFORM PASS                                  IF1064.2
            051300     ELSE                                                         IF1064.2
            051400                    MOVE WS-NUM TO COMPUTED-N                     IF1064.2
            051500                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1064.2
            051600                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1064.2
            051700                    PERFORM FAIL.                                 IF1064.2
            051800     GO TO F-COS-WRITE-08.                                        IF1064.2
            051900 F-COS-DELETE-08.                                                 IF1064.2
            052000     PERFORM  DE-LETE.                                            IF1064.2
            052100     GO TO    F-COS-WRITE-08.                                     IF1064.2
            052200 F-COS-WRITE-08.                                                  IF1064.2
            052300     MOVE "F-COS-08" TO PAR-NAME.                                 IF1064.2
            052400     PERFORM  PRINT-DETAIL.                                       IF1064.2
            052500*****************TEST (a) - COMPLEX TEST****************          IF1064.2
            052600 F-COS-09.                                                        IF1064.2
            052700     MOVE ZERO TO WS-NUM.                                         IF1064.2
            052800     MOVE  0.499980 TO MIN-RANGE.                                 IF1064.2
            052900     MOVE  0.500020 TO MAX-RANGE.                                 IF1064.2
            053000 F-COS-TEST-09.                                                   IF1064.2
            053100     COMPUTE WS-NUM = FUNCTION COS(PI / 3).                       IF1064.2
            053200     IF (WS-NUM >= MIN-RANGE) AND                                 IF1064.2
            053300        (WS-NUM <= MAX-RANGE) THEN                                IF1064.2
            053400                    PERFORM PASS                                  IF1064.2
            053500     ELSE                                                         IF1064.2
            053600                    MOVE WS-NUM TO COMPUTED-N                     IF1064.2
            053700                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1064.2
            053800                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1064.2
            053900                    PERFORM FAIL.                                 IF1064.2
            054000     GO TO F-COS-WRITE-09.                                        IF1064.2
            054100 F-COS-DELETE-09.                                                 IF1064.2
            054200     PERFORM  DE-LETE.                                            IF1064.2
            054300     GO TO    F-COS-WRITE-09.                                     IF1064.2
            054400 F-COS-WRITE-09.                                                  IF1064.2
            054500     MOVE "F-COS-09" TO PAR-NAME.                                 IF1064.2
            054600     PERFORM  PRINT-DETAIL.                                       IF1064.2
            054700*****************TEST (b) - COMPLEX TEST****************          IF1064.2
            054800 F-COS-10.                                                        IF1064.2
            054900     MOVE ZERO TO WS-NUM.                                         IF1064.2
            055000     MOVE -0.000040 TO MIN-RANGE.                                 IF1064.2
            055100     MOVE  0.000040 TO MAX-RANGE.                                 IF1064.2
            055200 F-COS-TEST-10.                                                   IF1064.2
            055300     COMPUTE WS-NUM = FUNCTION COS(PI / 2).                       IF1064.2
            055400     IF (WS-NUM >= MIN-RANGE) AND                                 IF1064.2
            055500        (WS-NUM <= MAX-RANGE) THEN                                IF1064.2
            055600                    PERFORM PASS                                  IF1064.2
            055700     ELSE                                                         IF1064.2
            055800                    MOVE WS-NUM TO COMPUTED-N                     IF1064.2
            055900                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1064.2
            056000                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1064.2
            056100                    PERFORM FAIL.                                 IF1064.2
            056200     GO TO F-COS-WRITE-10.                                        IF1064.2
            056300 F-COS-DELETE-10.                                                 IF1064.2
            056400     PERFORM  DE-LETE.                                            IF1064.2
            056500     GO TO    F-COS-WRITE-10.                                     IF1064.2
            056600 F-COS-WRITE-10.                                                  IF1064.2
            056700     MOVE "F-COS-10" TO PAR-NAME.                                 IF1064.2
            056800     PERFORM  PRINT-DETAIL.                                       IF1064.2
            056900*****************TEST (c) - COMPLEX TEST****************          IF1064.2
            057000 F-COS-11.                                                        IF1064.2
            057100     MOVE ZERO TO WS-NUM.                                         IF1064.2
            057200     MOVE -0.000040 TO MIN-RANGE.                                 IF1064.2
            057300     MOVE  0.000040 TO MAX-RANGE.                                 IF1064.2
            057400 F-COS-TEST-11.                                                   IF1064.2
            057500     COMPUTE WS-NUM = FUNCTION COS((3 * PI) / 2).                 IF1064.2
            057600     IF (WS-NUM >= MIN-RANGE) AND                                 IF1064.2
            057700        (WS-NUM <= MAX-RANGE) THEN                                IF1064.2
            057800                    PERFORM PASS                                  IF1064.2
            057900     ELSE                                                         IF1064.2
            058000                    MOVE WS-NUM TO COMPUTED-N                     IF1064.2
            058100                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1064.2
            058200                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1064.2
            058300                    PERFORM FAIL.                                 IF1064.2
            058400     GO TO F-COS-WRITE-11.                                        IF1064.2
            058500 F-COS-DELETE-11.                                                 IF1064.2
            058600     PERFORM  DE-LETE.                                            IF1064.2
            058700     GO TO    F-COS-WRITE-11.                                     IF1064.2
            058800 F-COS-WRITE-11.                                                  IF1064.2
            058900     MOVE "F-COS-11" TO PAR-NAME.                                 IF1064.2
            059000     PERFORM  PRINT-DETAIL.                                       IF1064.2
            059100*****************TEST (d) - COMPLEX TEST****************          IF1064.2
            059200 F-COS-12.                                                        IF1064.2
            059300     MOVE ZERO TO WS-NUM.                                         IF1064.2
            059400     MOVE  0.499980 TO MIN-RANGE.                                 IF1064.2
            059500     MOVE  0.500002 TO MAX-RANGE.                                 IF1064.2
            059600 F-COS-TEST-12.                                                   IF1064.2
            059700     COMPUTE WS-NUM = FUNCTION COS(MINUSPI / 3).                  IF1064.2
            059800     IF (WS-NUM >= MIN-RANGE) AND                                 IF1064.2
            059900        (WS-NUM <= MAX-RANGE) THEN                                IF1064.2
            060000                    PERFORM PASS                                  IF1064.2
            060100     ELSE                                                         IF1064.2
            060200                    MOVE WS-NUM TO COMPUTED-N                     IF1064.2
            060300                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1064.2
            060400                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1064.2
            060500                    PERFORM FAIL.                                 IF1064.2
            060600     GO TO F-COS-WRITE-12.                                        IF1064.2
            060700 F-COS-DELETE-12.                                                 IF1064.2
            060800     PERFORM  DE-LETE.                                            IF1064.2
            060900     GO TO    F-COS-WRITE-12.                                     IF1064.2
            061000 F-COS-WRITE-12.                                                  IF1064.2
            061100     MOVE "F-COS-12" TO PAR-NAME.                                 IF1064.2
            061200     PERFORM  PRINT-DETAIL.                                       IF1064.2
            061300*****************TEST (e) - COMPLEX TEST****************          IF1064.2
            061400 F-COS-13.                                                        IF1064.2
            061500     MOVE ZERO TO WS-NUM.                                         IF1064.2
            061600     MOVE -0.000040 TO MIN-RANGE.                                 IF1064.2
            061700     MOVE  0.000040 TO MAX-RANGE.                                 IF1064.2
            061800 F-COS-TEST-13.                                                   IF1064.2
            061900     COMPUTE WS-NUM = FUNCTION COS(MINUSPI / 2).                  IF1064.2
            062000     IF (WS-NUM >= MIN-RANGE) AND                                 IF1064.2
            062100        (WS-NUM <= MAX-RANGE) THEN                                IF1064.2
            062200                    PERFORM PASS                                  IF1064.2
            062300     ELSE                                                         IF1064.2
            062400                    MOVE WS-NUM TO COMPUTED-N                     IF1064.2
            062500                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1064.2
            062600                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1064.2
            062700                    PERFORM FAIL.                                 IF1064.2
            062800     GO TO F-COS-WRITE-13.                                        IF1064.2
            062900 F-COS-DELETE-13.                                                 IF1064.2
            063000     PERFORM  DE-LETE.                                            IF1064.2
            063100     GO TO    F-COS-WRITE-13.                                     IF1064.2
            063200 F-COS-WRITE-13.                                                  IF1064.2
            063300     MOVE "F-COS-13" TO PAR-NAME.                                 IF1064.2
            063400     PERFORM  PRINT-DETAIL.                                       IF1064.2
            063500*****************TEST (f) - COMPLEX TEST****************          IF1064.2
            063600 F-COS-14.                                                        IF1064.2
            063700     MOVE ZERO TO WS-NUM.                                         IF1064.2
            063800     MOVE -0.000040 TO MIN-RANGE.                                 IF1064.2
            063900     MOVE  0.000040 TO MAX-RANGE.                                 IF1064.2
            064000 F-COS-TEST-14.                                                   IF1064.2
            064100     COMPUTE WS-NUM = FUNCTION COS((3 * MINUSPI) / 2).            IF1064.2
            064200     IF (WS-NUM >= MIN-RANGE) AND                                 IF1064.2
            064300        (WS-NUM <= MAX-RANGE) THEN                                IF1064.2
            064400                    PERFORM PASS                                  IF1064.2
            064500     ELSE                                                         IF1064.2
            064600                    MOVE WS-NUM TO COMPUTED-N                     IF1064.2
            064700                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1064.2
            064800                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1064.2
            064900                    PERFORM FAIL.                                 IF1064.2
            065000     GO TO F-COS-WRITE-14.                                        IF1064.2
            065100 F-COS-DELETE-14.                                                 IF1064.2
            065200     PERFORM  DE-LETE.                                            IF1064.2
            065300     GO TO    F-COS-WRITE-14.                                     IF1064.2
            065400 F-COS-WRITE-14.                                                  IF1064.2
            065500     MOVE "F-COS-14" TO PAR-NAME.                                 IF1064.2
            065600     PERFORM  PRINT-DETAIL.                                       IF1064.2
            065700*****************TEST (h) - COMPLEX TEST****************          IF1064.2
            065800 F-COS-16.                                                        IF1064.2
            065900     MOVE ZERO TO WS-NUM.                                         IF1064.2
            066000     MOVE  0.499113 TO MIN-RANGE.                                 IF1064.2
            066100     MOVE  0.499153 TO MAX-RANGE.                                 IF1064.2
            066200 F-COS-TEST-16.                                                   IF1064.2
            066300     COMPUTE WS-NUM = FUNCTION COS((PI / 3) + 0.001).             IF1064.2
            066400     IF (WS-NUM >= MIN-RANGE) AND                                 IF1064.2
            066500        (WS-NUM <= MAX-RANGE) THEN                                IF1064.2
            066600                    PERFORM PASS                                  IF1064.2
            066700     ELSE                                                         IF1064.2
            066800                    MOVE WS-NUM TO COMPUTED-N                     IF1064.2
            066900                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1064.2
            067000                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1064.2
            067100                    PERFORM FAIL.                                 IF1064.2
            067200     GO TO F-COS-WRITE-16.                                        IF1064.2
            067300 F-COS-DELETE-16.                                                 IF1064.2
            067400     PERFORM  DE-LETE.                                            IF1064.2
            067500     GO TO    F-COS-WRITE-16.                                     IF1064.2
            067600 F-COS-WRITE-16.                                                  IF1064.2
            067700     MOVE "F-COS-16" TO PAR-NAME.                                 IF1064.2
            067800     PERFORM  PRINT-DETAIL.                                       IF1064.2
            067900*****************TEST (j) - COMPLEX TEST****************          IF1064.2
            068000 F-COS-18.                                                        IF1064.2
            068100     MOVE ZERO TO WS-NUM.                                         IF1064.2
            068200     MOVE  0.999350 TO MIN-RANGE.                                 IF1064.2
            068300     MOVE  0.999430 TO MAX-RANGE.                                 IF1064.2
            068400 F-COS-TEST-18.                                                   IF1064.2
            068500     COMPUTE WS-NUM = FUNCTION COS(PI * (4 - 2) / 180).           IF1064.2
            068600     IF (WS-NUM >= MIN-RANGE) AND                                 IF1064.2
            068700        (WS-NUM <= MAX-RANGE) THEN                                IF1064.2
            068800                    PERFORM PASS                                  IF1064.2
            068900     ELSE                                                         IF1064.2
            069000                    MOVE WS-NUM TO COMPUTED-N                     IF1064.2
            069100                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1064.2
            069200                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1064.2
            069300                    PERFORM FAIL.                                 IF1064.2
            069400     GO TO F-COS-WRITE-18.                                        IF1064.2
            069500 F-COS-DELETE-18.                                                 IF1064.2
            069600     PERFORM  DE-LETE.                                            IF1064.2
            069700     GO TO    F-COS-WRITE-18.                                     IF1064.2
            069800 F-COS-WRITE-18.                                                  IF1064.2
            069900     MOVE "F-COS-18" TO PAR-NAME.                                 IF1064.2
            070000     PERFORM  PRINT-DETAIL.                                       IF1064.2
            070100*****************TEST (k) - COMPLEX TEST****************          IF1064.2
            070200 F-COS-19.                                                        IF1064.2
            070300     MOVE ZERO TO WS-NUM.                                         IF1064.2
            070400     MOVE  0.017451 TO MIN-RANGE.                                 IF1064.2
            070500     MOVE  0.017453 TO MAX-RANGE.                                 IF1064.2
            070600 F-COS-TEST-19.                                                   IF1064.2
            070700     COMPUTE WS-NUM = FUNCTION COS((PI / 2) - (PI / 180)).        IF1064.2
            070800     IF (WS-NUM >= MIN-RANGE) AND                                 IF1064.2
            070900        (WS-NUM <= MAX-RANGE) THEN                                IF1064.2
            071000                    PERFORM PASS                                  IF1064.2
            071100     ELSE                                                         IF1064.2
            071200                    MOVE WS-NUM TO COMPUTED-N                     IF1064.2
            071300                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1064.2
            071400                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1064.2
            071500                    PERFORM FAIL.                                 IF1064.2
            071600     GO TO F-COS-WRITE-19.                                        IF1064.2
            071700 F-COS-DELETE-19.                                                 IF1064.2
            071800     PERFORM  DE-LETE.                                            IF1064.2
            071900     GO TO    F-COS-WRITE-19.                                     IF1064.2
            072000 F-COS-WRITE-19.                                                  IF1064.2
            072100     MOVE "F-COS-19" TO PAR-NAME.                                 IF1064.2
            072200     PERFORM  PRINT-DETAIL.                                       IF1064.2
            072300*****************TEST (l) - COMPLEX TEST****************          IF1064.2
            072400 F-COS-20.                                                        IF1064.2
            072500     MOVE ZERO TO WS-NUM.                                         IF1064.2
            072600     MOVE  0.515017  TO MIN-RANGE.                                IF1064.2
            072700     MOVE  0.515059 TO MAX-RANGE.                                 IF1064.2
            072800 F-COS-TEST-20.                                                   IF1064.2
            072900     COMPUTE WS-NUM = FUNCTION COS((PI / 3) - (PI / 180)).        IF1064.2
            073000     IF (WS-NUM >= MIN-RANGE) AND                                 IF1064.2
            073100        (WS-NUM <= MAX-RANGE) THEN                                IF1064.2
            073200                    PERFORM PASS                                  IF1064.2
            073300     ELSE                                                         IF1064.2
            073400                    MOVE WS-NUM TO COMPUTED-N                     IF1064.2
            073500                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1064.2
            073600                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1064.2
            073700                    PERFORM FAIL.                                 IF1064.2
            073800     GO TO F-COS-WRITE-20.                                        IF1064.2
            073900 F-COS-DELETE-20.                                                 IF1064.2
            074000     PERFORM  DE-LETE.                                            IF1064.2
            074100     GO TO    F-COS-WRITE-20.                                     IF1064.2
            074200 F-COS-WRITE-20.                                                  IF1064.2
            074300     MOVE "F-COS-20" TO PAR-NAME.                                 IF1064.2
            074400     PERFORM  PRINT-DETAIL.                                       IF1064.2
            074500*****************TEST (m) - COMPLEX TEST****************          IF1064.2
            074600 F-COS-21.                                                        IF1064.2
            074700     MOVE ZERO TO WS-NUM.                                         IF1064.2
            074800     MOVE -0.999887 TO MIN-RANGE.                                 IF1064.2
            074900     MOVE -0.999807 TO MAX-RANGE.                                 IF1064.2
            075000 F-COS-TEST-21.                                                   IF1064.2
            075100     COMPUTE WS-NUM = FUNCTION COS(PI + (PI / 180)).              IF1064.2
            075200     IF (WS-NUM >= MIN-RANGE) AND                                 IF1064.2
            075300        (WS-NUM <= MAX-RANGE) THEN                                IF1064.2
            075400                    PERFORM PASS                                  IF1064.2
            075500     ELSE                                                         IF1064.2
            075600                    MOVE WS-NUM TO COMPUTED-N                     IF1064.2
            075700                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1064.2
            075800                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1064.2
            075900                    PERFORM FAIL.                                 IF1064.2
            076000     GO TO F-COS-WRITE-21.                                        IF1064.2
            076100 F-COS-DELETE-21.                                                 IF1064.2
            076200     PERFORM  DE-LETE.                                            IF1064.2
            076300     GO TO    F-COS-WRITE-21.                                     IF1064.2
            076400 F-COS-WRITE-21.                                                  IF1064.2
            076500     MOVE "F-COS-21" TO PAR-NAME.                                 IF1064.2
            076600     PERFORM  PRINT-DETAIL.                                       IF1064.2
            076700*****************TEST (n) - COMPLEX TEST****************          IF1064.2
            076800 F-COS-22.                                                        IF1064.2
            076900     MOVE ZERO TO WS-NUM.                                         IF1064.2
            077000     MOVE  0.034898 TO MIN-RANGE.                                 IF1064.2
            077100     MOVE  0.034900 TO MAX-RANGE.                                 IF1064.2
            077200 F-COS-TEST-22.                                                   IF1064.2
            077300     COMPUTE WS-NUM = FUNCTION COS(( PI * 272) / 180).            IF1064.2
            077400     IF (WS-NUM >= MIN-RANGE) AND                                 IF1064.2
            077500        (WS-NUM <= MAX-RANGE) THEN                                IF1064.2
            077600                    PERFORM PASS                                  IF1064.2
            077700     ELSE                                                         IF1064.2
            077800                    MOVE WS-NUM TO COMPUTED-N                     IF1064.2
            077900                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1064.2
            078000                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1064.2
            078100                    PERFORM FAIL.                                 IF1064.2
            078200     GO TO F-COS-WRITE-22.                                        IF1064.2
            078300 F-COS-DELETE-22.                                                 IF1064.2
            078400     PERFORM  DE-LETE.                                            IF1064.2
            078500     GO TO    F-COS-WRITE-22.                                     IF1064.2
            078600 F-COS-WRITE-22.                                                  IF1064.2
            078700     MOVE "F-COS-22" TO PAR-NAME.                                 IF1064.2
            078800     PERFORM  PRINT-DETAIL.                                       IF1064.2
            078900*****************TEST (o) - COMPLEX TEST****************          IF1064.2
            079000 F-COS-23.                                                        IF1064.2
            079100     MOVE ZERO TO WS-NUM.                                         IF1064.2
            079200     MOVE -0.416163  TO MIN-RANGE.                                IF1064.2
            079300     MOVE -0.416129 TO MAX-RANGE.                                 IF1064.2
            079400 F-COS-TEST-23.                                                   IF1064.2
            079500     COMPUTE WS-NUM = FUNCTION COS(4 / 2).                        IF1064.2
            079600     IF (WS-NUM >= MIN-RANGE) AND                                 IF1064.2
            079700        (WS-NUM <= MAX-RANGE) THEN                                IF1064.2
            079800                    PERFORM PASS                                  IF1064.2
            079900     ELSE                                                         IF1064.2
            080000                    MOVE WS-NUM TO COMPUTED-N                     IF1064.2
            080100                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1064.2
            080200                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1064.2
            080300                    PERFORM FAIL.                                 IF1064.2
            080400     GO TO F-COS-WRITE-23.                                        IF1064.2
            080500 F-COS-DELETE-23.                                                 IF1064.2
            080600     PERFORM  DE-LETE.                                            IF1064.2
            080700     GO TO    F-COS-WRITE-23.                                     IF1064.2
            080800 F-COS-WRITE-23.                                                  IF1064.2
            080900     MOVE "F-COS-23" TO PAR-NAME.                                 IF1064.2
            081000     PERFORM  PRINT-DETAIL.                                       IF1064.2
            081100*****************TEST (p) - COMPLEX TEST****************          IF1064.2
            081200 F-COS-24.                                                        IF1064.2
            081300     MOVE ZERO TO WS-NUM.                                         IF1064.2
            081400     MOVE  0.070734 TO MIN-RANGE.                                 IF1064.2
            081500     MOVE  0.070740 TO MAX-RANGE.                                 IF1064.2
            081600 F-COS-TEST-24.                                                   IF1064.2
            081700     COMPUTE WS-NUM = FUNCTION COS(3 / 2).                        IF1064.2
            081800     IF (WS-NUM >= MIN-RANGE) AND                                 IF1064.2
            081900        (WS-NUM <= MAX-RANGE) THEN                                IF1064.2
            082000                    PERFORM PASS                                  IF1064.2
            082100     ELSE                                                         IF1064.2
            082200                    MOVE WS-NUM TO COMPUTED-N                     IF1064.2
            082300                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1064.2
            082400                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1064.2
            082500                    PERFORM FAIL.                                 IF1064.2
            082600     GO TO F-COS-WRITE-24.                                        IF1064.2
            082700 F-COS-DELETE-24.                                                 IF1064.2
            082800     PERFORM  DE-LETE.                                            IF1064.2
            082900     GO TO    F-COS-WRITE-24.                                     IF1064.2
            083000 F-COS-WRITE-24.                                                  IF1064.2
            083100     MOVE "F-COS-24" TO PAR-NAME.                                 IF1064.2
            083200     PERFORM  PRINT-DETAIL.                                       IF1064.2
            083300*****************TEST (q) - COMPLEX TEST****************          IF1064.2
            083400 F-COS-25.                                                        IF1064.2
            083500     MOVE ZERO TO WS-NUM.                                         IF1064.2
            083600     MOVE -1.000000 TO MIN-RANGE.                                 IF1064.2
            083700     MOVE -0.999960 TO MAX-RANGE.                                 IF1064.2
            083800 F-COS-TEST-25.                                                   IF1064.2
            083900     COMPUTE WS-NUM = FUNCTION COS(PI - A).                       IF1064.2
            084000     IF (WS-NUM >= MIN-RANGE) AND                                 IF1064.2
            084100        (WS-NUM <= MAX-RANGE) THEN                                IF1064.2
            084200                    PERFORM PASS                                  IF1064.2
            084300     ELSE                                                         IF1064.2
            084400                    MOVE WS-NUM TO COMPUTED-N                     IF1064.2
            084500                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1064.2
            084600                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1064.2
            084700                    PERFORM FAIL.                                 IF1064.2
            084800     GO TO F-COS-WRITE-25.                                        IF1064.2
            084900 F-COS-DELETE-25.                                                 IF1064.2
            085000     PERFORM  DE-LETE.                                            IF1064.2
            085100     GO TO    F-COS-WRITE-25.                                     IF1064.2
            085200 F-COS-WRITE-25.                                                  IF1064.2
            085300     MOVE "F-COS-25" TO PAR-NAME.                                 IF1064.2
            085400     PERFORM  PRINT-DETAIL.                                       IF1064.2
            085500*****************TEST (r) - COMPLEX TEST****************          IF1064.2
            085600 F-COS-26.                                                        IF1064.2
            085700     MOVE ZERO TO WS-NUM.                                         IF1064.2
            085800     MOVE -0.839105 TO MIN-RANGE.                                 IF1064.2
            085900     MOVE -0.839037 TO MAX-RANGE.                                 IF1064.2
            086000 F-COS-TEST-26.                                                   IF1064.2
            086100     COMPUTE WS-NUM = FUNCTION COS(D / 100).                      IF1064.2
            086200     IF (WS-NUM >= MIN-RANGE) AND                                 IF1064.2
            086300        (WS-NUM <= MAX-RANGE) THEN                                IF1064.2
            086400                    PERFORM PASS                                  IF1064.2
            086500     ELSE                                                         IF1064.2
            086600                    MOVE WS-NUM TO COMPUTED-N                     IF1064.2
            086700                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1064.2
            086800                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1064.2
            086900                    PERFORM FAIL.                                 IF1064.2
            087000     GO TO F-COS-WRITE-26.                                        IF1064.2
            087100 F-COS-DELETE-26.                                                 IF1064.2
            087200     PERFORM  DE-LETE.                                            IF1064.2
            087300     GO TO    F-COS-WRITE-26.                                     IF1064.2
            087400 F-COS-WRITE-26.                                                  IF1064.2
            087500     MOVE "F-COS-26" TO PAR-NAME.                                 IF1064.2
            087600     PERFORM  PRINT-DETAIL.                                       IF1064.2
            087700*****************TEST (s) - COMPLEX TEST****************          IF1064.2
            087800 F-COS-27.                                                        IF1064.2
            087900     MOVE ZERO TO WS-NUM.                                         IF1064.2
            088000     MOVE  0.999807  TO MIN-RANGE.                                IF1064.2
            088100     MOVE  0.999887 TO MAX-RANGE.                                 IF1064.2
            088200 F-COS-TEST-27.                                                   IF1064.2
            088300     COMPUTE WS-NUM = FUNCTION COS(PI / 180).                     IF1064.2
            088400     IF (WS-NUM >= MIN-RANGE) AND                                 IF1064.2
            088500        (WS-NUM <= MAX-RANGE) THEN                                IF1064.2
            088600                    PERFORM PASS                                  IF1064.2
            088700     ELSE                                                         IF1064.2
            088800                    MOVE WS-NUM TO COMPUTED-N                     IF1064.2
            088900                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1064.2
            089000                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1064.2
            089100                    PERFORM FAIL.                                 IF1064.2
            089200     GO TO F-COS-WRITE-27.                                        IF1064.2
            089300 F-COS-DELETE-27.                                                 IF1064.2
            089400     PERFORM  DE-LETE.                                            IF1064.2
            089500     GO TO    F-COS-WRITE-27.                                     IF1064.2
            089600 F-COS-WRITE-27.                                                  IF1064.2
            089700     MOVE "F-COS-27" TO PAR-NAME.                                 IF1064.2
            089800     PERFORM  PRINT-DETAIL.                                       IF1064.2
            089900*****************TEST (t) - COMPLEX TEST****************          IF1064.2
            090000 F-COS-28.                                                        IF1064.2
            090100     MOVE ZERO TO WS-NUM.                                         IF1064.2
            090200     MOVE -1.000000 TO MIN-RANGE.                                 IF1064.2
            090300     MOVE -0.999960 TO MAX-RANGE.                                 IF1064.2
            090400 F-COS-TEST-28.                                                   IF1064.2
            090500     COMPUTE WS-NUM = FUNCTION COS(PI - 0.001).                   IF1064.2
            090600     IF (WS-NUM >= MIN-RANGE) AND                                 IF1064.2
            090700        (WS-NUM <= MAX-RANGE) THEN                                IF1064.2
            090800                    PERFORM PASS                                  IF1064.2
            090900     ELSE                                                         IF1064.2
            091000                    MOVE WS-NUM TO COMPUTED-N                     IF1064.2
            091100                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1064.2
            091200                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1064.2
            091300                    PERFORM FAIL.                                 IF1064.2
            091400     GO TO F-COS-WRITE-28.                                        IF1064.2
            091500 F-COS-DELETE-28.                                                 IF1064.2
            091600     PERFORM  DE-LETE.                                            IF1064.2
            091700     GO TO    F-COS-WRITE-28.                                     IF1064.2
            091800 F-COS-WRITE-28.                                                  IF1064.2
            091900     MOVE "F-COS-28" TO PAR-NAME.                                 IF1064.2
            092000     PERFORM  PRINT-DETAIL.                                       IF1064.2
            092100*****************TEST (u) - COMPLEX TEST****************          IF1064.2
            092200 F-COS-29.                                                        IF1064.2
            092300     MOVE ZERO TO WS-NUM.                                         IF1064.2
            092400     MOVE -0.000040 TO MIN-RANGE.                                 IF1064.2
            092500     MOVE  0.000040 TO MAX-RANGE.                                 IF1064.2
            092600 F-COS-TEST-29.                                                   IF1064.2
            092700     COMPUTE WS-NUM = FUNCTION COS(PI) + 1.                       IF1064.2
            092800     IF (WS-NUM >= MIN-RANGE) AND                                 IF1064.2
            092900        (WS-NUM <= MAX-RANGE) THEN                                IF1064.2
            093000                    PERFORM PASS                                  IF1064.2
            093100     ELSE                                                         IF1064.2
            093200                    MOVE WS-NUM TO COMPUTED-N                     IF1064.2
            093300                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1064.2
            093400                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1064.2
            093500                    PERFORM FAIL.                                 IF1064.2
            093600     GO TO F-COS-WRITE-29.                                        IF1064.2
            093700 F-COS-DELETE-29.                                                 IF1064.2
            093800     PERFORM  DE-LETE.                                            IF1064.2
            093900     GO TO    F-COS-WRITE-29.                                     IF1064.2
            094000 F-COS-WRITE-29.                                                  IF1064.2
            094100     MOVE "F-COS-29" TO PAR-NAME.                                 IF1064.2
            094200     PERFORM  PRINT-DETAIL.                                       IF1064.2
            094300*****************TEST (v) - COMPLEX TEST****************          IF1064.2
            094400 F-COS-30.                                                        IF1064.2
            094500     MOVE ZERO TO WS-NUM.                                         IF1064.2
            094600     MOVE 0.914616 TO MIN-RANGE.                                  IF1064.2
            094700     MOVE 0.914690 TO MAX-RANGE.                                  IF1064.2
            094800 F-COS-TEST-30.                                                   IF1064.2
            094900     COMPUTE WS-NUM = FUNCTION COS(FUNCTION COS(2)).              IF1064.2
            095000     IF (WS-NUM >= MIN-RANGE) AND                                 IF1064.2
            095100        (WS-NUM <= MAX-RANGE) THEN                                IF1064.2
            095200                    PERFORM PASS                                  IF1064.2
            095300     ELSE                                                         IF1064.2
            095400                    MOVE WS-NUM TO COMPUTED-N                     IF1064.2
            095500                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1064.2
            095600                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1064.2
            095700                    PERFORM FAIL.                                 IF1064.2
            095800     GO TO F-COS-WRITE-30.                                        IF1064.2
            095900 F-COS-DELETE-30.                                                 IF1064.2
            096000     PERFORM  DE-LETE.                                            IF1064.2
            096100     GO TO    F-COS-WRITE-30.                                     IF1064.2
            096200 F-COS-WRITE-30.                                                  IF1064.2
            096300     MOVE "F-COS-30" TO PAR-NAME.                                 IF1064.2
            096400     PERFORM  PRINT-DETAIL.                                       IF1064.2
            096500*****************TEST (w) - COMPLEX TEST****************          IF1064.2
            096600 F-COS-31.                                                        IF1064.2
            096700     MOVE ZERO TO WS-NUM.                                         IF1064.2
            096800     MOVE -2.00008 TO MIN-RANGE.                                  IF1064.2
            096900     MOVE -1.99992 TO MAX-RANGE.                                  IF1064.2
            097000 F-COS-TEST-31.                                                   IF1064.2
            097100     COMPUTE WS-NUM = FUNCTION COS(PI) +                          IF1064.2
            097200                              FUNCTION COS(PI).                   IF1064.2
            097300     IF (WS-NUM >= MIN-RANGE) AND                                 IF1064.2
            097400        (WS-NUM <= MAX-RANGE) THEN                                IF1064.2
            097500                    PERFORM PASS                                  IF1064.2
            097600     ELSE                                                         IF1064.2
            097700                    MOVE WS-NUM TO COMPUTED-N                     IF1064.2
            097800                    MOVE MIN-RANGE TO CORRECT-MIN                 IF1064.2
            097900                    MOVE MAX-RANGE TO CORRECT-MAX                 IF1064.2
            098000                    PERFORM FAIL.                                 IF1064.2
            098100     GO TO F-COS-WRITE-31.                                        IF1064.2
            098200 F-COS-DELETE-31.                                                 IF1064.2
            098300     PERFORM  DE-LETE.                                            IF1064.2
            098400     GO TO    F-COS-WRITE-31.                                     IF1064.2
            098500 F-COS-WRITE-31.                                                  IF1064.2
            098600     MOVE "F-COS-31" TO PAR-NAME.                                 IF1064.2
            098700     PERFORM  PRINT-DETAIL.                                       IF1064.2
            098800*****************SPECIAL PERFORM TEST**********************       IF1064.2
            098900 F-COS-32.                                                        IF1064.2
            099000     PERFORM F-COS-TEST-32                                        IF1064.2
            099100       UNTIL FUNCTION COS(ARG1) < 0.                              IF1064.2
            099200     PERFORM PASS.                                                IF1064.2
            099300     GO TO F-COS-WRITE-32.                                        IF1064.2
            099400 F-COS-TEST-32.                                                   IF1064.2
            099500     COMPUTE ARG1 = ARG1 - 0.25.                                  IF1064.2
            099600 F-COS-DELETE-32.                                                 IF1064.2
            099700     PERFORM  DE-LETE.                                            IF1064.2
            099800     GO TO    F-COS-WRITE-32.                                     IF1064.2
            099900 F-COS-WRITE-32.                                                  IF1064.2
            100000     MOVE "F-COS-32" TO PAR-NAME.                                 IF1064.2
            100100     PERFORM  PRINT-DETAIL.                                       IF1064.2
            100200********************END OF TESTS***************                   IF1064.2
            100300 CCVS-EXIT SECTION.                                               IF1064.2
            100400 CCVS-999999.                                                     IF1064.2
            100500     GO TO CLOSE-FILES.                                           IF1064.2
                  *END-OF,IF106A                                                                  
        """)
    )

    @Test
    fun if1074_2() = rewriteRun(
        cobol("""
                  *HEADER,COBOL,IF107A                                                            
            000100 IDENTIFICATION DIVISION.                                         IF1074.2
            000200 PROGRAM-ID.                                                      IF1074.2
            000300     IF107A.                                                      IF1074.2
            000400                                                                  IF1074.2
            000500***********************************************************       IF1074.2
            000600*                                                         *       IF1074.2
            000700* This program forms part of the CCVS85 COBOL Test Suite. *       IF1074.2
            000800* It contains tests for the Intrinsic Function            *       IF1074.2
            000900* CURRENT-DATE.                                           *       IF1074.2
            001000*                                                         *       IF1074.2
            001100***********************************************************       IF1074.2
            001200 ENVIRONMENT DIVISION.                                            IF1074.2
            001300 CONFIGURATION SECTION.                                           IF1074.2
            001400 SOURCE-COMPUTER.                                                 IF1074.2
            001500     XXXXX082.                                                    IF1074.2
            001600 OBJECT-COMPUTER.                                                 IF1074.2
            001700     XXXXX083.                                                    IF1074.2
            001800 INPUT-OUTPUT SECTION.                                            IF1074.2
            001900 FILE-CONTROL.                                                    IF1074.2
            002000     SELECT PRINT-FILE ASSIGN TO                                  IF1074.2
            002100     XXXXX055.                                                    IF1074.2
            002200 DATA DIVISION.                                                   IF1074.2
            002300 FILE SECTION.                                                    IF1074.2
            002400 FD  PRINT-FILE.                                                  IF1074.2
            002500 01  PRINT-REC PICTURE X(120).                                    IF1074.2
            002600 01  DUMMY-RECORD PICTURE X(120).                                 IF1074.2
            002700 WORKING-STORAGE SECTION.                                         IF1074.2
            002800***********************************************************       IF1074.2
            002900* Variables specific to the Intrinsic Function Test IF107A*       IF1074.2
            003000***********************************************************       IF1074.2
            003100 01  TEMP1                       PIC X(21).                       IF1074.2
            003200 01  TEMP2                       PIC X(21).                       IF1074.2
            003300 01  WS-FIRST                    VALUE SPACES.                    IF1074.2
            003400     02  FILLER                  PIC X(8).                        IF1074.2
            003500     02  WS-TIME1                PIC X(8).                        IF1074.2
            003600     02  FILLER                  PIC X(5).                        IF1074.2
            003700 01  WS-SECOND                   VALUE SPACES.                    IF1074.2
            003800     02  FILLER                  PIC X(8).                        IF1074.2
            003900     02  WS-TIME2                PIC X(8).                        IF1074.2
            004000     02  FILLER                  PIC X(5).                        IF1074.2
            004100 01  WS-DATE.                                                     IF1074.2
            004200     02  WS-YEAR                 PIC 9999.                        IF1074.2
            004300              88 CON-YEAR        VALUE 1990 THRU 9999.            IF1074.2
            004400     02  WS-MONTH                PIC 99.                          IF1074.2
            004500              88 CON-MONTH       VALUE 01 THRU 12.                IF1074.2
            004600     02  WS-DAY                  PIC 99.                          IF1074.2
            004700              88 CON-DAY         VALUE 01 THRU 31.                IF1074.2
            004800     02  WS-HOUR                 PIC 99.                          IF1074.2
            004900              88 CON-HOUR        VALUE 00 THRU 23.                IF1074.2
            005000     02  WS-MIN                  PIC 99.                          IF1074.2
            005100              88 CON-MIN         VALUE 00 THRU 59.                IF1074.2
            005200     02  WS-SECOND               PIC 99.                          IF1074.2
            005300              88 CON-SEC         VALUE 00 THRU 59.                IF1074.2
            005400     02  WS-HUNDSEC              PIC 99.                          IF1074.2
            005500              88 CON-HUNDSEC     VALUE 00 THRU 99.                IF1074.2
            005600     02  WS-GREENW               PIC X.                           IF1074.2
            005700              88 CON-GREENW      VALUE "-", "+", "0".             IF1074.2
            005800     02  WS-OFFSET               PIC 99.                          IF1074.2
            005900              88 CON-OFFSET      VALUE 00 THRU 13.                IF1074.2
            006000     02  WS-OFFSET2              PIC 99.                          IF1074.2
            006100              88 CON-OFFSET2     VALUE 00 THRU 59.                IF1074.2
            006200*                                                                 IF1074.2
            006300**********************************************************        IF1074.2
            006400*                                                                 IF1074.2
            006500 01  TEST-RESULTS.                                                IF1074.2
            006600     02 FILLER                   PIC X      VALUE SPACE.          IF1074.2
            006700     02 FEATURE                  PIC X(20)  VALUE SPACE.          IF1074.2
            006800     02 FILLER                   PIC X      VALUE SPACE.          IF1074.2
            006900     02 P-OR-F                   PIC X(5)   VALUE SPACE.          IF1074.2
            007000     02 FILLER                   PIC X      VALUE SPACE.          IF1074.2
            007100     02  PAR-NAME.                                                IF1074.2
            007200       03 FILLER                 PIC X(19)  VALUE SPACE.          IF1074.2
            007300       03  PARDOT-X              PIC X      VALUE SPACE.          IF1074.2
            007400       03 DOTVALUE               PIC 99     VALUE ZERO.           IF1074.2
            007500     02 FILLER                   PIC X(8)   VALUE SPACE.          IF1074.2
            007600     02 RE-MARK                  PIC X(61).                       IF1074.2
            007700 01  TEST-COMPUTED.                                               IF1074.2
            007800     02 FILLER                   PIC X(30)  VALUE SPACE.          IF1074.2
            007900     02 FILLER                   PIC X(17)  VALUE                 IF1074.2
            008000            "       COMPUTED=".                                   IF1074.2
            008100     02 COMPUTED-X.                                               IF1074.2
            008200     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          IF1074.2
            008300     03 COMPUTED-N               REDEFINES COMPUTED-A             IF1074.2
            008400                                 PIC -9(9).9(9).                  IF1074.2
            008500     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         IF1074.2
            008600     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     IF1074.2
            008700     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     IF1074.2
            008800     03       CM-18V0 REDEFINES COMPUTED-A.                       IF1074.2
            008900         04 COMPUTED-18V0                    PIC -9(18).          IF1074.2
            009000         04 FILLER                           PIC X.               IF1074.2
            009100     03 FILLER PIC X(50) VALUE SPACE.                             IF1074.2
            009200 01  TEST-CORRECT.                                                IF1074.2
            009300     02 FILLER PIC X(30) VALUE SPACE.                             IF1074.2
            009400     02 FILLER PIC X(17) VALUE "       CORRECT =".                IF1074.2
            009500     02 CORRECT-X.                                                IF1074.2
            009600     03 CORRECT-A                  PIC X(20) VALUE SPACE.         IF1074.2
            009700     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      IF1074.2
            009800     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         IF1074.2
            009900     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     IF1074.2
            010000     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     IF1074.2
            010100     03      CR-18V0 REDEFINES CORRECT-A.                         IF1074.2
            010200         04 CORRECT-18V0                     PIC -9(18).          IF1074.2
            010300         04 FILLER                           PIC X.               IF1074.2
            010400     03 FILLER PIC X(2) VALUE SPACE.                              IF1074.2
            010500     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     IF1074.2
            010600 01  CCVS-C-1.                                                    IF1074.2
            010700     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PAIF1074.2
            010800-    "SS  PARAGRAPH-NAME                                          IF1074.2
            010900-    "       REMARKS".                                            IF1074.2
            011000     02 FILLER                     PIC X(20)    VALUE SPACE.      IF1074.2
            011100 01  CCVS-C-2.                                                    IF1074.2
            011200     02 FILLER                     PIC X        VALUE SPACE.      IF1074.2
            011300     02 FILLER                     PIC X(6)     VALUE "TESTED".   IF1074.2
            011400     02 FILLER                     PIC X(15)    VALUE SPACE.      IF1074.2
            011500     02 FILLER                     PIC X(4)     VALUE "FAIL".     IF1074.2
            011600     02 FILLER                     PIC X(94)    VALUE SPACE.      IF1074.2
            011700 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       IF1074.2
            011800 01  REC-CT                        PIC 99       VALUE ZERO.       IF1074.2
            011900 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       IF1074.2
            012000 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       IF1074.2
            012100 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       IF1074.2
            012200 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       IF1074.2
            012300 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       IF1074.2
            012400 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       IF1074.2
            012500 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      IF1074.2
            012600 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       IF1074.2
            012700 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     IF1074.2
            012800 01  CCVS-H-1.                                                    IF1074.2
            012900     02  FILLER                    PIC X(39)    VALUE SPACES.     IF1074.2
            013000     02  FILLER                    PIC X(42)    VALUE             IF1074.2
            013100     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 IF1074.2
            013200     02  FILLER                    PIC X(39)    VALUE SPACES.     IF1074.2
            013300 01  CCVS-H-2A.                                                   IF1074.2
            013400   02  FILLER                        PIC X(40)  VALUE SPACE.      IF1074.2
            013500   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  IF1074.2
            013600   02  FILLER                        PIC XXXX   VALUE             IF1074.2
            013700     "4.2 ".                                                      IF1074.2
            013800   02  FILLER                        PIC X(28)  VALUE             IF1074.2
            013900            " COPY - NOT FOR DISTRIBUTION".                       IF1074.2
            014000   02  FILLER                        PIC X(41)  VALUE SPACE.      IF1074.2
            014100                                                                  IF1074.2
            014200 01  CCVS-H-2B.                                                   IF1074.2
            014300   02  FILLER                        PIC X(15)  VALUE             IF1074.2
            014400            "TEST RESULT OF ".                                    IF1074.2
            014500   02  TEST-ID                       PIC X(9).                    IF1074.2
            014600   02  FILLER                        PIC X(4)   VALUE             IF1074.2
            014700            " IN ".                                               IF1074.2
            014800   02  FILLER                        PIC X(12)  VALUE             IF1074.2
            014900     " HIGH       ".                                              IF1074.2
            015000   02  FILLER                        PIC X(22)  VALUE             IF1074.2
            015100            " LEVEL VALIDATION FOR ".                             IF1074.2
            015200   02  FILLER                        PIC X(58)  VALUE             IF1074.2
            015300     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".IF1074.2
            015400 01  CCVS-H-3.                                                    IF1074.2
            015500     02  FILLER                      PIC X(34)  VALUE             IF1074.2
            015600            " FOR OFFICIAL USE ONLY    ".                         IF1074.2
            015700     02  FILLER                      PIC X(58)  VALUE             IF1074.2
            015800     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".IF1074.2
            015900     02  FILLER                      PIC X(28)  VALUE             IF1074.2
            016000            "  COPYRIGHT   1985 ".                                IF1074.2
            016100 01  CCVS-E-1.                                                    IF1074.2
            016200     02 FILLER                       PIC X(52)  VALUE SPACE.      IF1074.2
            016300     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              IF1074.2
            016400     02 ID-AGAIN                     PIC X(9).                    IF1074.2
            016500     02 FILLER                       PIC X(45)  VALUE SPACES.     IF1074.2
            016600 01  CCVS-E-2.                                                    IF1074.2
            016700     02  FILLER                      PIC X(31)  VALUE SPACE.      IF1074.2
            016800     02  FILLER                      PIC X(21)  VALUE SPACE.      IF1074.2
            016900     02 CCVS-E-2-2.                                               IF1074.2
            017000         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      IF1074.2
            017100         03 FILLER                   PIC X      VALUE SPACE.      IF1074.2
            017200         03 ENDER-DESC               PIC X(44)  VALUE             IF1074.2
            017300            "ERRORS ENCOUNTERED".                                 IF1074.2
            017400 01  CCVS-E-3.                                                    IF1074.2
            017500     02  FILLER                      PIC X(22)  VALUE             IF1074.2
            017600            " FOR OFFICIAL USE ONLY".                             IF1074.2
            017700     02  FILLER                      PIC X(12)  VALUE SPACE.      IF1074.2
            017800     02  FILLER                      PIC X(58)  VALUE             IF1074.2
            017900     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".IF1074.2
            018000     02  FILLER                      PIC X(13)  VALUE SPACE.      IF1074.2
            018100     02 FILLER                       PIC X(15)  VALUE             IF1074.2
            018200             " COPYRIGHT 1985".                                   IF1074.2
            018300 01  CCVS-E-4.                                                    IF1074.2
            018400     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      IF1074.2
            018500     02 FILLER                       PIC X(4)   VALUE " OF ".     IF1074.2
            018600     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      IF1074.2
            018700     02 FILLER                       PIC X(40)  VALUE             IF1074.2
            018800      "  TESTS WERE EXECUTED SUCCESSFULLY".                       IF1074.2
            018900 01  XXINFO.                                                      IF1074.2
            019000     02 FILLER                       PIC X(19)  VALUE             IF1074.2
            019100            "*** INFORMATION ***".                                IF1074.2
            019200     02 INFO-TEXT.                                                IF1074.2
            019300       04 FILLER                     PIC X(8)   VALUE SPACE.      IF1074.2
            019400       04 XXCOMPUTED                 PIC X(20).                   IF1074.2
            019500       04 FILLER                     PIC X(5)   VALUE SPACE.      IF1074.2
            019600       04 XXCORRECT                  PIC X(20).                   IF1074.2
            019700     02 INF-ANSI-REFERENCE           PIC X(48).                   IF1074.2
            019800 01  HYPHEN-LINE.                                                 IF1074.2
            019900     02 FILLER  PIC IS X VALUE IS SPACE.                          IF1074.2
            020000     02 FILLER  PIC IS X(65)    VALUE IS "************************IF1074.2
            020100-    "*****************************************".                 IF1074.2
            020200     02 FILLER  PIC IS X(54)    VALUE IS "************************IF1074.2
            020300-    "******************************".                            IF1074.2
            020400 01  CCVS-PGM-ID                     PIC X(9)   VALUE             IF1074.2
            020500     "IF107A".                                                    IF1074.2
            020600 PROCEDURE DIVISION.                                              IF1074.2
            020700 CCVS1 SECTION.                                                   IF1074.2
            020800 OPEN-FILES.                                                      IF1074.2
            020900     OPEN     OUTPUT PRINT-FILE.                                  IF1074.2
            021000     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   IF1074.2
            021100     MOVE    SPACE TO TEST-RESULTS.                               IF1074.2
            021200     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             IF1074.2
            021300     GO TO CCVS1-EXIT.                                            IF1074.2
            021400 CLOSE-FILES.                                                     IF1074.2
            021500     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   IF1074.2
            021600 TERMINATE-CCVS.                                                  IF1074.2
            021700     STOP     RUN.                                                IF1074.2
            021800 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         IF1074.2
            021900 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           IF1074.2
            022000 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          IF1074.2
            022100 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      IF1074.2
            022200     MOVE "****TEST DELETED****" TO RE-MARK.                      IF1074.2
            022300 PRINT-DETAIL.                                                    IF1074.2
            022400     IF REC-CT NOT EQUAL TO ZERO                                  IF1074.2
            022500             MOVE "." TO PARDOT-X                                 IF1074.2
            022600             MOVE REC-CT TO DOTVALUE.                             IF1074.2
            022700     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      IF1074.2
            022800     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               IF1074.2
            022900        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 IF1074.2
            023000          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 IF1074.2
            023100     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              IF1074.2
            023200     MOVE SPACE TO CORRECT-X.                                     IF1074.2
            023300     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         IF1074.2
            023400     MOVE     SPACE TO RE-MARK.                                   IF1074.2
            023500 HEAD-ROUTINE.                                                    IF1074.2
            023600     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  IF1074.2
            023700     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  IF1074.2
            023800     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  IF1074.2
            023900     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  IF1074.2
            024000 COLUMN-NAMES-ROUTINE.                                            IF1074.2
            024100     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           IF1074.2
            024200     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1074.2
            024300     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        IF1074.2
            024400 END-ROUTINE.                                                     IF1074.2
            024500     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5       IF1074.2
            024600     TIMES.                                                       IF1074.2
            024700 END-RTN-EXIT.                                                    IF1074.2
            024800     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1074.2
            024900 END-ROUTINE-1.                                                   IF1074.2
            025000      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      IF1074.2
            025100      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               IF1074.2
            025200      ADD PASS-COUNTER TO ERROR-HOLD.                             IF1074.2
            025300      MOVE PASS-COUNTER TO CCVS-E-4-1.                            IF1074.2
            025400      MOVE ERROR-HOLD TO CCVS-E-4-2.                              IF1074.2
            025500      MOVE CCVS-E-4 TO CCVS-E-2-2.                                IF1074.2
            025600      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           IF1074.2
            025700  END-ROUTINE-12.                                                 IF1074.2
            025800      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        IF1074.2
            025900     IF       ERROR-COUNTER IS EQUAL TO ZERO                      IF1074.2
            026000         MOVE "NO " TO ERROR-TOTAL                                IF1074.2
            026100         ELSE                                                     IF1074.2
            026200         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       IF1074.2
            026300     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           IF1074.2
            026400     PERFORM WRITE-LINE.                                          IF1074.2
            026500 END-ROUTINE-13.                                                  IF1074.2
            026600     IF DELETE-COUNTER IS EQUAL TO ZERO                           IF1074.2
            026700         MOVE "NO " TO ERROR-TOTAL  ELSE                          IF1074.2
            026800         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      IF1074.2
            026900     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   IF1074.2
            027000     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           IF1074.2
            027100      IF   INSPECT-COUNTER EQUAL TO ZERO                          IF1074.2
            027200          MOVE "NO " TO ERROR-TOTAL                               IF1074.2
            027300      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   IF1074.2
            027400      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            IF1074.2
            027500      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          IF1074.2
            027600     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           IF1074.2
            027700 WRITE-LINE.                                                      IF1074.2
            027800     ADD 1 TO RECORD-COUNT.                                       IF1074.2
            027900*    IF RECORD-COUNT GREATER 42                                   IF1074.2
            028000*        MOVE DUMMY-RECORD TO DUMMY-HOLD                          IF1074.2
            028100*        MOVE SPACE TO DUMMY-RECORD                               IF1074.2
            028200*        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  IF1074.2
            028300*        MOVE CCVS-H-1  TO DUMMY-RECORD  PERFORM WRT-LN 2 TIMES   IF1074.2
            028400*        MOVE CCVS-H-2A TO DUMMY-RECORD  PERFORM WRT-LN 2 TIMES   IF1074.2
            028500*        MOVE CCVS-H-2B TO DUMMY-RECORD  PERFORM WRT-LN 3 TIMES   IF1074.2
            028600*        MOVE CCVS-H-3  TO DUMMY-RECORD  PERFORM WRT-LN 3 TIMES   IF1074.2
            028700*        MOVE CCVS-C-1  TO DUMMY-RECORD  PERFORM WRT-LN           IF1074.2
            028800*        MOVE CCVS-C-2  TO DUMMY-RECORD  PERFORM WRT-LN           IF1074.2
            028900*        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          IF1074.2
            029000*        MOVE DUMMY-HOLD TO DUMMY-RECORD                          IF1074.2
            029100*        MOVE ZERO TO RECORD-COUNT.                               IF1074.2
            029200     PERFORM WRT-LN.                                              IF1074.2
            029300 WRT-LN.                                                          IF1074.2
            029400     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               IF1074.2
            029500     MOVE SPACE TO DUMMY-RECORD.                                  IF1074.2
            029600 BLANK-LINE-PRINT.                                                IF1074.2
            029700     PERFORM WRT-LN.                                              IF1074.2
            029800 FAIL-ROUTINE.                                                    IF1074.2
            029900     IF     COMPUTED-X NOT EQUAL TO SPACE                         IF1074.2
            030000            GO TO FAIL-ROUTINE-WRITE.                             IF1074.2
            030100     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.IF1074.2
            030200     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 IF1074.2
            030300     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   IF1074.2
            030400     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1074.2
            030500     MOVE   SPACES TO INF-ANSI-REFERENCE.                         IF1074.2
            030600     GO TO  FAIL-ROUTINE-EX.                                      IF1074.2
            030700 FAIL-ROUTINE-WRITE.                                              IF1074.2
            030800     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         IF1074.2
            030900     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 IF1074.2
            031000     MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. IF1074.2
            031100     MOVE   SPACES TO COR-ANSI-REFERENCE.                         IF1074.2
            031200 FAIL-ROUTINE-EX. EXIT.                                           IF1074.2
            031300 BAIL-OUT.                                                        IF1074.2
            031400     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   IF1074.2
            031500     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           IF1074.2
            031600 BAIL-OUT-WRITE.                                                  IF1074.2
            031700     MOVE CORRECT-A TO XXCORRECT.                                 IF1074.2
            031800     MOVE COMPUTED-A TO XXCOMPUTED.                               IF1074.2
            031900     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 IF1074.2
            032000     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1074.2
            032100     MOVE   SPACES TO INF-ANSI-REFERENCE.                         IF1074.2
            032200 BAIL-OUT-EX. EXIT.                                               IF1074.2
            032300 CCVS1-EXIT.                                                      IF1074.2
            032400     EXIT.                                                        IF1074.2
            032500********************************************************          IF1074.2
            032600*                                                      *          IF1074.2
            032700*    Intrinsic Function Tests   IF107A - CURRENT-DATE *           IF1074.2
            032800*                                                      *          IF1074.2
            032900********************************************************          IF1074.2
            033000 SECT-IF107A SECTION.                                             IF1074.2
            033100 F-WHENCOMP-INFO.                                                 IF1074.2
            033200     MOVE     "See ref. A-39 2.11" TO ANSI-REFERENCE.             IF1074.2
            033300     MOVE     "CURRENT-DATE"     TO FEATURE.                      IF1074.2
            033400*****************TEST (a) ******************************          IF1074.2
            033500 F-WHENCOMP-01.                                                   IF1074.2
            033600     MOVE SPACES TO TEMP1.                                        IF1074.2
            033700     MOVE SPACES TO WS-DATE.                                      IF1074.2
            033800 F-WHENCOMP-TEST-01.                                              IF1074.2
            033900     MOVE FUNCTION CURRENT-DATE TO TEMP1.                         IF1074.2
            034000     MOVE TEMP1 TO WS-DATE.                                       IF1074.2
            034100     IF CON-YEAR     AND                                          IF1074.2
            034200        CON-MONTH    AND                                          IF1074.2
            034300        CON-DAY      AND                                          IF1074.2
            034400        CON-HOUR     AND                                          IF1074.2
            034500        CON-MIN      AND                                          IF1074.2
            034600        CON-SEC      AND                                          IF1074.2
            034700        CON-HUNDSEC  AND                                          IF1074.2
            034800        CON-GREENW   AND                                          IF1074.2
            034900        CON-OFFSET   AND                                          IF1074.2
            035000        CON-OFFSET2  THEN                                         IF1074.2
            035100                  PERFORM PASS                                    IF1074.2
            035200     ELSE                                                         IF1074.2
            035300                  MOVE TEMP1 TO COMPUTED-A                        IF1074.2
            035400                  MOVE "Date & Time value " TO CORRECT-X          IF1074.2
            035500                  PERFORM FAIL.                                   IF1074.2
            035600     GO TO F-WHENCOMP-WRITE-01.                                   IF1074.2
            035700 F-WHENCOMP-DELETE-01.                                            IF1074.2
            035800     PERFORM  DE-LETE.                                            IF1074.2
            035900     GO TO    F-WHENCOMP-WRITE-01.                                IF1074.2
            036000 F-WHENCOMP-WRITE-01.                                             IF1074.2
            036100     MOVE "F-WHENCOMP-01" TO PAR-NAME.                            IF1074.2
            036200     PERFORM  PRINT-DETAIL.                                       IF1074.2
            036300*****************TEST (b) ******************************          IF1074.2
            036400 F-WHENCOMP-TEST-02.                                              IF1074.2
            036500     IF FUNCTION CURRENT-DATE >= TEMP1 THEN                       IF1074.2
            036600                    PERFORM PASS                                  IF1074.2
            036700     ELSE                                                         IF1074.2
            036800                    PERFORM FAIL.                                 IF1074.2
            036900     GO TO F-WHENCOMP-WRITE-02.                                   IF1074.2
            037000 F-WHENCOMP-DELETE-02.                                            IF1074.2
            037100     PERFORM  DE-LETE.                                            IF1074.2
            037200     GO TO    F-WHENCOMP-WRITE-02.                                IF1074.2
            037300 F-WHENCOMP-WRITE-02.                                             IF1074.2
            037400     MOVE "F-WHENCOMP-02" TO PAR-NAME.                            IF1074.2
            037500     PERFORM  PRINT-DETAIL.                                       IF1074.2
            037600*******************END OF TESTS**************************         IF1074.2
            037700 CCVS-EXIT SECTION.                                               IF1074.2
            037800 CCVS-999999.                                                     IF1074.2
            037900     GO TO CLOSE-FILES.                                           IF1074.2
                  *END-OF,IF107A                                                                  
        """)
    )

    @Test
    fun if1084_2() = rewriteRun(
        cobol("""
                  *HEADER,COBOL,IF108A                                                            
            000100 IDENTIFICATION DIVISION.                                         IF1084.2
            000200 PROGRAM-ID.                                                      IF1084.2
            000300     IF108A.                                                      IF1084.2
            000400                                                                  IF1084.2
            000500***********************************************************       IF1084.2
            000600*                                                         *       IF1084.2
            000700* This program forms part of the CCVS85 COBOL Test Suite. *       IF1084.2
            000800* It contains tests for the Intrinsic Function            *       IF1084.2
            000900* DATE-OF-INTEGER.                                        *       IF1084.2
            001000*                                                         *       IF1084.2
            001100***********************************************************       IF1084.2
            001200 ENVIRONMENT DIVISION.                                            IF1084.2
            001300 CONFIGURATION SECTION.                                           IF1084.2
            001400 SOURCE-COMPUTER.                                                 IF1084.2
            001500     XXXXX082.                                                    IF1084.2
            001600 OBJECT-COMPUTER.                                                 IF1084.2
            001700     XXXXX083.                                                    IF1084.2
            001800 INPUT-OUTPUT SECTION.                                            IF1084.2
            001900 FILE-CONTROL.                                                    IF1084.2
            002000     SELECT PRINT-FILE ASSIGN TO                                  IF1084.2
            002100     XXXXX055.                                                    IF1084.2
            002200 DATA DIVISION.                                                   IF1084.2
            002300 FILE SECTION.                                                    IF1084.2
            002400 FD  PRINT-FILE.                                                  IF1084.2
            002500 01  PRINT-REC PICTURE X(120).                                    IF1084.2
            002600 01  DUMMY-RECORD PICTURE X(120).                                 IF1084.2
            002700 WORKING-STORAGE SECTION.                                         IF1084.2
            002800***********************************************************       IF1084.2
            002900* Variables specific to the Intrinsic Function Test IF108A*       IF1084.2
            003000***********************************************************       IF1084.2
            003100 01  A                   PIC S9(10)     VALUE 400.                IF1084.2
            003200 01  C                   PIC S9(10)     VALUE 300.                IF1084.2
            003300 01  D                   PIC S9(10)     VALUE 1.                  IF1084.2
            003400 01  ARG1                PIC S9(10)     VALUE 1.                  IF1084.2
            003500 01  ARR                                VALUE "40537".            IF1084.2
            003600     02 IND OCCURS 5 TIMES PIC 9.                                 IF1084.2
            003700 01  TEMP                PIC S9(5)V9(5).                          IF1084.2
            003800 01  WS-DATE             PIC 9(8).                                IF1084.2
            003900*                                                                 IF1084.2
            004000**********************************************************        IF1084.2
            004100*                                                                 IF1084.2
            004200 01  TEST-RESULTS.                                                IF1084.2
            004300     02 FILLER                   PIC X      VALUE SPACE.          IF1084.2
            004400     02 FEATURE                  PIC X(20)  VALUE SPACE.          IF1084.2
            004500     02 FILLER                   PIC X      VALUE SPACE.          IF1084.2
            004600     02 P-OR-F                   PIC X(5)   VALUE SPACE.          IF1084.2
            004700     02 FILLER                   PIC X      VALUE SPACE.          IF1084.2
            004800     02  PAR-NAME.                                                IF1084.2
            004900       03 FILLER                 PIC X(19)  VALUE SPACE.          IF1084.2
            005000       03  PARDOT-X              PIC X      VALUE SPACE.          IF1084.2
            005100       03 DOTVALUE               PIC 99     VALUE ZERO.           IF1084.2
            005200     02 FILLER                   PIC X(8)   VALUE SPACE.          IF1084.2
            005300     02 RE-MARK                  PIC X(61).                       IF1084.2
            005400 01  TEST-COMPUTED.                                               IF1084.2
            005500     02 FILLER                   PIC X(30)  VALUE SPACE.          IF1084.2
            005600     02 FILLER                   PIC X(17)  VALUE                 IF1084.2
            005700            "       COMPUTED=".                                   IF1084.2
            005800     02 COMPUTED-X.                                               IF1084.2
            005900     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          IF1084.2
            006000     03 COMPUTED-N               REDEFINES COMPUTED-A             IF1084.2
            006100                                 PIC -9(9).9(9).                  IF1084.2
            006200     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         IF1084.2
            006300     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     IF1084.2
            006400     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     IF1084.2
            006500     03       CM-18V0 REDEFINES COMPUTED-A.                       IF1084.2
            006600         04 COMPUTED-18V0                    PIC -9(18).          IF1084.2
            006700         04 FILLER                           PIC X.               IF1084.2
            006800     03 FILLER PIC X(50) VALUE SPACE.                             IF1084.2
            006900 01  TEST-CORRECT.                                                IF1084.2
            007000     02 FILLER PIC X(30) VALUE SPACE.                             IF1084.2
            007100     02 FILLER PIC X(17) VALUE "       CORRECT =".                IF1084.2
            007200     02 CORRECT-X.                                                IF1084.2
            007300     03 CORRECT-A                  PIC X(20) VALUE SPACE.         IF1084.2
            007400     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      IF1084.2
            007500     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         IF1084.2
            007600     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     IF1084.2
            007700     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     IF1084.2
            007800     03      CR-18V0 REDEFINES CORRECT-A.                         IF1084.2
            007900         04 CORRECT-18V0                     PIC -9(18).          IF1084.2
            008000         04 FILLER                           PIC X.               IF1084.2
            008100     03 FILLER PIC X(2) VALUE SPACE.                              IF1084.2
            008200     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     IF1084.2
            008300 01  CCVS-C-1.                                                    IF1084.2
            008400     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PAIF1084.2
            008500-    "SS  PARAGRAPH-NAME                                          IF1084.2
            008600-    "       REMARKS".                                            IF1084.2
            008700     02 FILLER                     PIC X(20)    VALUE SPACE.      IF1084.2
            008800 01  CCVS-C-2.                                                    IF1084.2
            008900     02 FILLER                     PIC X        VALUE SPACE.      IF1084.2
            009000     02 FILLER                     PIC X(6)     VALUE "TESTED".   IF1084.2
            009100     02 FILLER                     PIC X(15)    VALUE SPACE.      IF1084.2
            009200     02 FILLER                     PIC X(4)     VALUE "FAIL".     IF1084.2
            009300     02 FILLER                     PIC X(94)    VALUE SPACE.      IF1084.2
            009400 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       IF1084.2
            009500 01  REC-CT                        PIC 99       VALUE ZERO.       IF1084.2
            009600 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       IF1084.2
            009700 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       IF1084.2
            009800 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       IF1084.2
            009900 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       IF1084.2
            010000 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       IF1084.2
            010100 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       IF1084.2
            010200 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      IF1084.2
            010300 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       IF1084.2
            010400 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     IF1084.2
            010500 01  CCVS-H-1.                                                    IF1084.2
            010600     02  FILLER                    PIC X(39)    VALUE SPACES.     IF1084.2
            010700     02  FILLER                    PIC X(42)    VALUE             IF1084.2
            010800     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 IF1084.2
            010900     02  FILLER                    PIC X(39)    VALUE SPACES.     IF1084.2
            011000 01  CCVS-H-2A.                                                   IF1084.2
            011100   02  FILLER                        PIC X(40)  VALUE SPACE.      IF1084.2
            011200   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  IF1084.2
            011300   02  FILLER                        PIC XXXX   VALUE             IF1084.2
            011400     "4.2 ".                                                      IF1084.2
            011500   02  FILLER                        PIC X(28)  VALUE             IF1084.2
            011600            " COPY - NOT FOR DISTRIBUTION".                       IF1084.2
            011700   02  FILLER                        PIC X(41)  VALUE SPACE.      IF1084.2
            011800                                                                  IF1084.2
            011900 01  CCVS-H-2B.                                                   IF1084.2
            012000   02  FILLER                        PIC X(15)  VALUE             IF1084.2
            012100            "TEST RESULT OF ".                                    IF1084.2
            012200   02  TEST-ID                       PIC X(9).                    IF1084.2
            012300   02  FILLER                        PIC X(4)   VALUE             IF1084.2
            012400            " IN ".                                               IF1084.2
            012500   02  FILLER                        PIC X(12)  VALUE             IF1084.2
            012600     " HIGH       ".                                              IF1084.2
            012700   02  FILLER                        PIC X(22)  VALUE             IF1084.2
            012800            " LEVEL VALIDATION FOR ".                             IF1084.2
            012900   02  FILLER                        PIC X(58)  VALUE             IF1084.2
            013000     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".IF1084.2
            013100 01  CCVS-H-3.                                                    IF1084.2
            013200     02  FILLER                      PIC X(34)  VALUE             IF1084.2
            013300            " FOR OFFICIAL USE ONLY    ".                         IF1084.2
            013400     02  FILLER                      PIC X(58)  VALUE             IF1084.2
            013500     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".IF1084.2
            013600     02  FILLER                      PIC X(28)  VALUE             IF1084.2
            013700            "  COPYRIGHT   1985 ".                                IF1084.2
            013800 01  CCVS-E-1.                                                    IF1084.2
            013900     02 FILLER                       PIC X(52)  VALUE SPACE.      IF1084.2
            014000     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              IF1084.2
            014100     02 ID-AGAIN                     PIC X(9).                    IF1084.2
            014200     02 FILLER                       PIC X(45)  VALUE SPACES.     IF1084.2
            014300 01  CCVS-E-2.                                                    IF1084.2
            014400     02  FILLER                      PIC X(31)  VALUE SPACE.      IF1084.2
            014500     02  FILLER                      PIC X(21)  VALUE SPACE.      IF1084.2
            014600     02 CCVS-E-2-2.                                               IF1084.2
            014700         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      IF1084.2
            014800         03 FILLER                   PIC X      VALUE SPACE.      IF1084.2
            014900         03 ENDER-DESC               PIC X(44)  VALUE             IF1084.2
            015000            "ERRORS ENCOUNTERED".                                 IF1084.2
            015100 01  CCVS-E-3.                                                    IF1084.2
            015200     02  FILLER                      PIC X(22)  VALUE             IF1084.2
            015300            " FOR OFFICIAL USE ONLY".                             IF1084.2
            015400     02  FILLER                      PIC X(12)  VALUE SPACE.      IF1084.2
            015500     02  FILLER                      PIC X(58)  VALUE             IF1084.2
            015600     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".IF1084.2
            015700     02  FILLER                      PIC X(13)  VALUE SPACE.      IF1084.2
            015800     02 FILLER                       PIC X(15)  VALUE             IF1084.2
            015900             " COPYRIGHT 1985".                                   IF1084.2
            016000 01  CCVS-E-4.                                                    IF1084.2
            016100     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      IF1084.2
            016200     02 FILLER                       PIC X(4)   VALUE " OF ".     IF1084.2
            016300     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      IF1084.2
            016400     02 FILLER                       PIC X(40)  VALUE             IF1084.2
            016500      "  TESTS WERE EXECUTED SUCCESSFULLY".                       IF1084.2
            016600 01  XXINFO.                                                      IF1084.2
            016700     02 FILLER                       PIC X(19)  VALUE             IF1084.2
            016800            "*** INFORMATION ***".                                IF1084.2
            016900     02 INFO-TEXT.                                                IF1084.2
            017000       04 FILLER                     PIC X(8)   VALUE SPACE.      IF1084.2
            017100       04 XXCOMPUTED                 PIC X(20).                   IF1084.2
            017200       04 FILLER                     PIC X(5)   VALUE SPACE.      IF1084.2
            017300       04 XXCORRECT                  PIC X(20).                   IF1084.2
            017400     02 INF-ANSI-REFERENCE           PIC X(48).                   IF1084.2
            017500 01  HYPHEN-LINE.                                                 IF1084.2
            017600     02 FILLER  PIC IS X VALUE IS SPACE.                          IF1084.2
            017700     02 FILLER  PIC IS X(65)    VALUE IS "************************IF1084.2
            017800-    "*****************************************".                 IF1084.2
            017900     02 FILLER  PIC IS X(54)    VALUE IS "************************IF1084.2
            018000-    "******************************".                            IF1084.2
            018100 01  CCVS-PGM-ID                     PIC X(9)   VALUE             IF1084.2
            018200     "IF108A".                                                    IF1084.2
            018300 PROCEDURE DIVISION.                                              IF1084.2
            018400 CCVS1 SECTION.                                                   IF1084.2
            018500 OPEN-FILES.                                                      IF1084.2
            018600     OPEN     OUTPUT PRINT-FILE.                                  IF1084.2
            018700     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   IF1084.2
            018800     MOVE    SPACE TO TEST-RESULTS.                               IF1084.2
            018900     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             IF1084.2
            019000     GO TO CCVS1-EXIT.                                            IF1084.2
            019100 CLOSE-FILES.                                                     IF1084.2
            019200     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   IF1084.2
            019300 TERMINATE-CCVS.                                                  IF1084.2
            019400     STOP     RUN.                                                IF1084.2
            019500 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         IF1084.2
            019600 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           IF1084.2
            019700 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          IF1084.2
            019800 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      IF1084.2
            019900     MOVE "****TEST DELETED****" TO RE-MARK.                      IF1084.2
            020000 PRINT-DETAIL.                                                    IF1084.2
            020100     IF REC-CT NOT EQUAL TO ZERO                                  IF1084.2
            020200             MOVE "." TO PARDOT-X                                 IF1084.2
            020300             MOVE REC-CT TO DOTVALUE.                             IF1084.2
            020400     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      IF1084.2
            020500     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               IF1084.2
            020600        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 IF1084.2
            020700          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 IF1084.2
            020800     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              IF1084.2
            020900     MOVE SPACE TO CORRECT-X.                                     IF1084.2
            021000     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         IF1084.2
            021100     MOVE     SPACE TO RE-MARK.                                   IF1084.2
            021200 HEAD-ROUTINE.                                                    IF1084.2
            021300     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  IF1084.2
            021400     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  IF1084.2
            021500     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  IF1084.2
            021600     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  IF1084.2
            021700 COLUMN-NAMES-ROUTINE.                                            IF1084.2
            021800     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           IF1084.2
            021900     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1084.2
            022000     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        IF1084.2
            022100 END-ROUTINE.                                                     IF1084.2
            022200     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.IF1084.2
            022300 END-RTN-EXIT.                                                    IF1084.2
            022400     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1084.2
            022500 END-ROUTINE-1.                                                   IF1084.2
            022600      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      IF1084.2
            022700      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               IF1084.2
            022800      ADD PASS-COUNTER TO ERROR-HOLD.                             IF1084.2
            022900      MOVE PASS-COUNTER TO CCVS-E-4-1.                            IF1084.2
            023000      MOVE ERROR-HOLD TO CCVS-E-4-2.                              IF1084.2
            023100      MOVE CCVS-E-4 TO CCVS-E-2-2.                                IF1084.2
            023200      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           IF1084.2
            023300  END-ROUTINE-12.                                                 IF1084.2
            023400      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        IF1084.2
            023500     IF       ERROR-COUNTER IS EQUAL TO ZERO                      IF1084.2
            023600         MOVE "NO " TO ERROR-TOTAL                                IF1084.2
            023700         ELSE                                                     IF1084.2
            023800         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       IF1084.2
            023900     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           IF1084.2
            024000     PERFORM WRITE-LINE.                                          IF1084.2
            024100 END-ROUTINE-13.                                                  IF1084.2
            024200     IF DELETE-COUNTER IS EQUAL TO ZERO                           IF1084.2
            024300         MOVE "NO " TO ERROR-TOTAL  ELSE                          IF1084.2
            024400         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      IF1084.2
            024500     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   IF1084.2
            024600     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           IF1084.2
            024700      IF   INSPECT-COUNTER EQUAL TO ZERO                          IF1084.2
            024800          MOVE "NO " TO ERROR-TOTAL                               IF1084.2
            024900      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   IF1084.2
            025000      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            IF1084.2
            025100      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          IF1084.2
            025200     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           IF1084.2
            025300 WRITE-LINE.                                                      IF1084.2
            025400     ADD 1 TO RECORD-COUNT.                                       IF1084.2
            025500*    IF RECORD-COUNT GREATER 42                                   IF1084.2
            025600*        MOVE DUMMY-RECORD TO DUMMY-HOLD                          IF1084.2
            025700*        MOVE SPACE TO DUMMY-RECORD                               IF1084.2
            025800*        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  IF1084.2
            025900*        MOVE CCVS-H-1  TO DUMMY-RECORD  PERFORM WRT-LN 2 TIMES   IF1084.2
            026000*        MOVE CCVS-H-2A TO DUMMY-RECORD  PERFORM WRT-LN 2 TIMES   IF1084.2
            026100*        MOVE CCVS-H-2B TO DUMMY-RECORD  PERFORM WRT-LN 3 TIMES   IF1084.2
            026200*        MOVE CCVS-H-3  TO DUMMY-RECORD  PERFORM WRT-LN 3 TIMES   IF1084.2
            026300*        MOVE CCVS-C-1  TO DUMMY-RECORD  PERFORM WRT-LN           IF1084.2
            026400*        MOVE CCVS-C-2  TO DUMMY-RECORD  PERFORM WRT-LN           IF1084.2
            026500*        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          IF1084.2
            026600*        MOVE DUMMY-HOLD TO DUMMY-RECORD                          IF1084.2
            026700*        MOVE ZERO TO RECORD-COUNT.                               IF1084.2
            026800     PERFORM WRT-LN.                                              IF1084.2
            026900 WRT-LN.                                                          IF1084.2
            027000     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               IF1084.2
            027100     MOVE SPACE TO DUMMY-RECORD.                                  IF1084.2
            027200 BLANK-LINE-PRINT.                                                IF1084.2
            027300     PERFORM WRT-LN.                                              IF1084.2
            027400 FAIL-ROUTINE.                                                    IF1084.2
            027500     IF     COMPUTED-X NOT EQUAL TO SPACE                         IF1084.2
            027600            GO TO FAIL-ROUTINE-WRITE.                             IF1084.2
            027700     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.IF1084.2
            027800     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 IF1084.2
            027900     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   IF1084.2
            028000     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1084.2
            028100     MOVE   SPACES TO INF-ANSI-REFERENCE.                         IF1084.2
            028200     GO TO  FAIL-ROUTINE-EX.                                      IF1084.2
            028300 FAIL-ROUTINE-WRITE.                                              IF1084.2
            028400     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         IF1084.2
            028500     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 IF1084.2
            028600     MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. IF1084.2
            028700     MOVE   SPACES TO COR-ANSI-REFERENCE.                         IF1084.2
            028800 FAIL-ROUTINE-EX. EXIT.                                           IF1084.2
            028900 BAIL-OUT.                                                        IF1084.2
            029000     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   IF1084.2
            029100     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           IF1084.2
            029200 BAIL-OUT-WRITE.                                                  IF1084.2
            029300     MOVE CORRECT-A TO XXCORRECT.                                 IF1084.2
            029400     MOVE COMPUTED-A TO XXCOMPUTED.                               IF1084.2
            029500     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 IF1084.2
            029600     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1084.2
            029700     MOVE   SPACES TO INF-ANSI-REFERENCE.                         IF1084.2
            029800 BAIL-OUT-EX. EXIT.                                               IF1084.2
            029900 CCVS1-EXIT.                                                      IF1084.2
            030000     EXIT.                                                        IF1084.2
            030100********************************************************          IF1084.2
            030200*                                                      *          IF1084.2
            030300*    Intrinsic Function Test  IF108A - DATE-OF-INTEGER *          IF1084.2
            030400*                                                      *          IF1084.2
            030500********************************************************          IF1084.2
            030600 SECT-IF108A SECTION.                                             IF1084.2
            030700 F-DATEOFINT-INFO.                                                IF1084.2
            030800     MOVE     "See ref. A-41 2.12" TO ANSI-REFERENCE.             IF1084.2
            030900     MOVE     "DATE-OF-INTEGER"     TO FEATURE.                   IF1084.2
            031000*****************TEST (a) ******************************          IF1084.2
            031100 F-DATEOFINT-01.                                                  IF1084.2
            031200     MOVE ZERO TO WS-DATE.                                        IF1084.2
            031300 F-DATEOFINT-TEST-01.                                             IF1084.2
            031400     COMPUTE WS-DATE = FUNCTION DATE-OF-INTEGER(1).               IF1084.2
            031500     IF WS-DATE = 16010101 THEN                                   IF1084.2
            031600                        PERFORM PASS                              IF1084.2
            031700     ELSE                                                         IF1084.2
            031800                        MOVE  16010101  TO CORRECT-N              IF1084.2
            031900                        MOVE WS-DATE TO COMPUTED-N                IF1084.2
            032000                        PERFORM FAIL.                             IF1084.2
            032100     GO TO F-DATEOFINT-WRITE-01.                                  IF1084.2
            032200 F-DATEOFINT-DELETE-01.                                           IF1084.2
            032300     PERFORM  DE-LETE.                                            IF1084.2
            032400     GO TO    F-DATEOFINT-WRITE-01.                               IF1084.2
            032500 F-DATEOFINT-WRITE-01.                                            IF1084.2
            032600     MOVE "F-DATEOFINT-01" TO PAR-NAME.                           IF1084.2
            032700     PERFORM  PRINT-DETAIL.                                       IF1084.2
            032800*****************TEST (b) ******************************          IF1084.2
            032900 F-DATEOFINT-TEST-02.                                             IF1084.2
            033000     EVALUATE FUNCTION DATE-OF-INTEGER(A)                         IF1084.2
            033100     WHEN     16020204                                            IF1084.2
            033200                    PERFORM PASS                                  IF1084.2
            033300                    GO TO F-DATEOFINT-WRITE-02.                   IF1084.2
            033400     PERFORM FAIL.                                                IF1084.2
            033500     GO TO F-DATEOFINT-WRITE-02.                                  IF1084.2
            033600 F-DATEOFINT-DELETE-02.                                           IF1084.2
            033700     PERFORM  DE-LETE.                                            IF1084.2
            033800     GO TO    F-DATEOFINT-WRITE-02.                               IF1084.2
            033900 F-DATEOFINT-WRITE-02.                                            IF1084.2
            034000     MOVE "F-DATEOFINT-02" TO PAR-NAME.                           IF1084.2
            034100     PERFORM  PRINT-DETAIL.                                       IF1084.2
            034200*****************TEST (c) ******************************          IF1084.2
            034300 F-DATEOFINT-TEST-03.                                             IF1084.2
            034400     IF FUNCTION DATE-OF-INTEGER(IND(1)) = 16010104 THEN          IF1084.2
            034500                        PERFORM PASS                              IF1084.2
            034600     ELSE                                                         IF1084.2
            034700                        PERFORM FAIL.                             IF1084.2
            034800     GO TO F-DATEOFINT-WRITE-03.                                  IF1084.2
            034900 F-DATEOFINT-DELETE-03.                                           IF1084.2
            035000     PERFORM  DE-LETE.                                            IF1084.2
            035100     GO TO    F-DATEOFINT-WRITE-03.                               IF1084.2
            035200 F-DATEOFINT-WRITE-03.                                            IF1084.2
            035300     MOVE "F-DATEOFINT-03" TO PAR-NAME.                           IF1084.2
            035400     PERFORM  PRINT-DETAIL.                                       IF1084.2
            035500*****************TEST (d) ******************************          IF1084.2
            035600 F-DATEOFINT-04.                                                  IF1084.2
            035700     MOVE ZERO TO WS-DATE.                                        IF1084.2
            035800 F-DATEOFINT-TEST-04.                                             IF1084.2
            035900     COMPUTE WS-DATE = FUNCTION DATE-OF-INTEGER(IND(D)).          IF1084.2
            036000     IF WS-DATE = 16010104 THEN                                   IF1084.2
            036100                        PERFORM PASS                              IF1084.2
            036200     ELSE                                                         IF1084.2
            036300                        MOVE  16010104  TO CORRECT-N              IF1084.2
            036400                        MOVE WS-DATE TO COMPUTED-N                IF1084.2
            036500                        PERFORM FAIL.                             IF1084.2
            036600     GO TO F-DATEOFINT-WRITE-04.                                  IF1084.2
            036700 F-DATEOFINT-DELETE-04.                                           IF1084.2
            036800     PERFORM  DE-LETE.                                            IF1084.2
            036900     GO TO    F-DATEOFINT-WRITE-04.                               IF1084.2
            037000 F-DATEOFINT-WRITE-04.                                            IF1084.2
            037100     MOVE "F-DATEOFINT-04" TO PAR-NAME.                           IF1084.2
            037200     PERFORM  PRINT-DETAIL.                                       IF1084.2
            037300*****************TEST (e) ******************************          IF1084.2
            037400 F-DATEOFINT-05.                                                  IF1084.2
            037500     MOVE ZERO TO WS-DATE.                                        IF1084.2
            037600 F-DATEOFINT-TEST-05.                                             IF1084.2
            037700     COMPUTE WS-DATE = FUNCTION DATE-OF-INTEGER(730).             IF1084.2
            037800     IF WS-DATE = 16021231 THEN                                   IF1084.2
            037900                        PERFORM PASS                              IF1084.2
            038000     ELSE                                                         IF1084.2
            038100                        MOVE  16021231  TO CORRECT-N              IF1084.2
            038200                        MOVE WS-DATE TO COMPUTED-N                IF1084.2
            038300                        PERFORM FAIL.                             IF1084.2
            038400     GO TO F-DATEOFINT-WRITE-05.                                  IF1084.2
            038500 F-DATEOFINT-DELETE-05.                                           IF1084.2
            038600     PERFORM  DE-LETE.                                            IF1084.2
            038700     GO TO    F-DATEOFINT-WRITE-05.                               IF1084.2
            038800 F-DATEOFINT-WRITE-05.                                            IF1084.2
            038900     MOVE "F-DATEOFINT-05" TO PAR-NAME.                           IF1084.2
            039000     PERFORM  PRINT-DETAIL.                                       IF1084.2
            039100*****************TEST (f) ******************************          IF1084.2
            039200 F-DATEOFINT-06.                                                  IF1084.2
            039300     MOVE ZERO TO WS-DATE.                                        IF1084.2
            039400 F-DATEOFINT-TEST-06.                                             IF1084.2
            039500     COMPUTE WS-DATE = FUNCTION DATE-OF-INTEGER(C).               IF1084.2
            039600     IF WS-DATE = 16011027 THEN                                   IF1084.2
            039700                        PERFORM PASS                              IF1084.2
            039800     ELSE                                                         IF1084.2
            039900                        MOVE  16011027  TO CORRECT-N              IF1084.2
            040000                        MOVE WS-DATE TO COMPUTED-N                IF1084.2
            040100                        PERFORM FAIL.                             IF1084.2
            040200     GO TO F-DATEOFINT-WRITE-06.                                  IF1084.2
            040300 F-DATEOFINT-DELETE-06.                                           IF1084.2
            040400     PERFORM  DE-LETE.                                            IF1084.2
            040500     GO TO    F-DATEOFINT-WRITE-06.                               IF1084.2
            040600 F-DATEOFINT-WRITE-06.                                            IF1084.2
            040700     MOVE "F-DATEOFINT-06" TO PAR-NAME.                           IF1084.2
            040800     PERFORM  PRINT-DETAIL.                                       IF1084.2
            040900*****************TEST (g) ******************************          IF1084.2
            041000 F-DATEOFINT-07.                                                  IF1084.2
            041100     MOVE ZERO TO WS-DATE.                                        IF1084.2
            041200 F-DATEOFINT-TEST-07.                                             IF1084.2
            041300     COMPUTE WS-DATE = FUNCTION DATE-OF-INTEGER(365).             IF1084.2
            041400     IF WS-DATE = 16011231 THEN                                   IF1084.2
            041500                        PERFORM PASS                              IF1084.2
            041600     ELSE                                                         IF1084.2
            041700                        MOVE  16011231  TO CORRECT-N              IF1084.2
            041800                        MOVE WS-DATE TO COMPUTED-N                IF1084.2
            041900                        PERFORM FAIL.                             IF1084.2
            042000     GO TO F-DATEOFINT-WRITE-07.                                  IF1084.2
            042100 F-DATEOFINT-DELETE-07.                                           IF1084.2
            042200     PERFORM  DE-LETE.                                            IF1084.2
            042300     GO TO    F-DATEOFINT-WRITE-07.                               IF1084.2
            042400 F-DATEOFINT-WRITE-07.                                            IF1084.2
            042500     MOVE "F-DATEOFINT-07" TO PAR-NAME.                           IF1084.2
            042600     PERFORM  PRINT-DETAIL.                                       IF1084.2
            042700*****************TEST (h) ******************************          IF1084.2
            042800 F-DATEOFINT-08.                                                  IF1084.2
            042900     MOVE ZERO TO WS-DATE.                                        IF1084.2
            043000 F-DATEOFINT-TEST-08.                                             IF1084.2
            043100     COMPUTE WS-DATE = FUNCTION DATE-OF-INTEGER(D) + 10.          IF1084.2
            043200     IF WS-DATE = 16010111 THEN                                   IF1084.2
            043300                        PERFORM PASS                              IF1084.2
            043400     ELSE                                                         IF1084.2
            043500                        MOVE  16010111  TO CORRECT-N              IF1084.2
            043600                        MOVE WS-DATE TO COMPUTED-N                IF1084.2
            043700                        PERFORM FAIL.                             IF1084.2
            043800     GO TO F-DATEOFINT-WRITE-08.                                  IF1084.2
            043900 F-DATEOFINT-DELETE-08.                                           IF1084.2
            044000     PERFORM  DE-LETE.                                            IF1084.2
            044100     GO TO    F-DATEOFINT-WRITE-08.                               IF1084.2
            044200 F-DATEOFINT-WRITE-08.                                            IF1084.2
            044300     MOVE "F-DATEOFINT-08" TO PAR-NAME.                           IF1084.2
            044400     PERFORM  PRINT-DETAIL.                                       IF1084.2
            044500*****************TEST (i) ******************************          IF1084.2
            044600 F-DATEOFINT-09.                                                  IF1084.2
            044700     MOVE ZERO TO WS-DATE.                                        IF1084.2
            044800 F-DATEOFINT-TEST-09.                                             IF1084.2
            044900     COMPUTE WS-DATE = FUNCTION DATE-OF-INTEGER(D) +              IF1084.2
            045000                    FUNCTION DATE-OF-INTEGER(D).                  IF1084.2
            045100     IF WS-DATE = 32020202 THEN                                   IF1084.2
            045200                        PERFORM PASS                              IF1084.2
            045300     ELSE                                                         IF1084.2
            045400                        MOVE  32020202  TO CORRECT-N              IF1084.2
            045500                        MOVE WS-DATE TO COMPUTED-N                IF1084.2
            045600                        PERFORM FAIL.                             IF1084.2
            045700     GO TO F-DATEOFINT-WRITE-09.                                  IF1084.2
            045800 F-DATEOFINT-DELETE-09.                                           IF1084.2
            045900     PERFORM  DE-LETE.                                            IF1084.2
            046000     GO TO    F-DATEOFINT-WRITE-09.                               IF1084.2
            046100 F-DATEOFINT-WRITE-09.                                            IF1084.2
            046200     MOVE "F-DATEOFINT-09" TO PAR-NAME.                           IF1084.2
            046300     PERFORM  PRINT-DETAIL.                                       IF1084.2
            046400                                                                  IF1084.2
            046500***************** SPECIAL TEST 1 ***********************          IF1084.2
            046600                                                                  IF1084.2
            046700 F-DATEOFINT-10.                                                  IF1084.2
            046800     MOVE 1 TO ARG1.                                              IF1084.2
            046900     PERFORM F-DATEOFINT-TEST-10                                  IF1084.2
            047000             UNTIL FUNCTION DATE-OF-INTEGER(ARG1) > 16010110.     IF1084.2
            047100     IF ARG1 = 11 THEN                                            IF1084.2
            047200                            PERFORM PASS                          IF1084.2
            047300     ELSE                                                         IF1084.2
            047400                            PERFORM FAIL.                         IF1084.2
            047500     GO TO F-DATEOFINT-WRITE-10.                                  IF1084.2
            047600*                                                                 IF1084.2
            047700 F-DATEOFINT-TEST-10.                                             IF1084.2
            047800     COMPUTE ARG1 = ARG1 + 1.                                     IF1084.2
            047900*                                                                 IF1084.2
            048000 F-DATEOFINT-DELETE-10.                                           IF1084.2
            048100     PERFORM  DE-LETE.                                            IF1084.2
            048200     GO TO    F-DATEOFINT-WRITE-10.                               IF1084.2
            048300 F-DATEOFINT-WRITE-10.                                            IF1084.2
            048400     MOVE "F-DATEOFINT-10" TO PAR-NAME.                           IF1084.2
            048500     PERFORM  PRINT-DETAIL.                                       IF1084.2
            048600*******************END OF TESTS**************************         IF1084.2
            048700 CCVS-EXIT SECTION.                                               IF1084.2
            048800 CCVS-999999.                                                     IF1084.2
            048900     GO TO CLOSE-FILES.                                           IF1084.2
                  *END-OF,IF108A                                                                  
        """)
    )

    @Test
    fun if1094_2() = rewriteRun(
        cobol("""
                  *HEADER,COBOL,IF109A                                                            
            000100 IDENTIFICATION DIVISION.                                         IF1094.2
            000200 PROGRAM-ID.                                                      IF1094.2
            000300     IF109A.                                                      IF1094.2
            000400                                                                  IF1094.2
            000500***********************************************************       IF1094.2
            000600*                                                         *       IF1094.2
            000700* This program forms part of the CCVS85 COBOL Test Suite. *       IF1094.2
            000800* It contains tests for the Intrinsic Function            *       IF1094.2
            000900* DAY-OF-INTEGER.                                         *       IF1094.2
            001000*                                                         *       IF1094.2
            001100***********************************************************       IF1094.2
            001200 ENVIRONMENT DIVISION.                                            IF1094.2
            001300 CONFIGURATION SECTION.                                           IF1094.2
            001400 SOURCE-COMPUTER.                                                 IF1094.2
            001500     XXXXX082.                                                    IF1094.2
            001600 OBJECT-COMPUTER.                                                 IF1094.2
            001700     XXXXX083.                                                    IF1094.2
            001800 INPUT-OUTPUT SECTION.                                            IF1094.2
            001900 FILE-CONTROL.                                                    IF1094.2
            002000     SELECT PRINT-FILE ASSIGN TO                                  IF1094.2
            002100     XXXXX055.                                                    IF1094.2
            002200 DATA DIVISION.                                                   IF1094.2
            002300 FILE SECTION.                                                    IF1094.2
            002400 FD  PRINT-FILE.                                                  IF1094.2
            002500 01  PRINT-REC PICTURE X(120).                                    IF1094.2
            002600 01  DUMMY-RECORD PICTURE X(120).                                 IF1094.2
            002700 WORKING-STORAGE SECTION.                                         IF1094.2
            002800***********************************************************       IF1094.2
            002900* Variables specific to the Intrinsic Function Test IF109A*       IF1094.2
            003000***********************************************************       IF1094.2
            003100 01  A                   PIC S9(10)     VALUE 400.                IF1094.2
            003200 01  C                   PIC S9(10)     VALUE 365.                IF1094.2
            003300 01  D                   PIC S9(10)     VALUE 1.                  IF1094.2
            003400 01  ARG1                PIC S9(10)     VALUE 1.                  IF1094.2
            003500 01  ARR                                VALUE "40537".            IF1094.2
            003600     02 IND OCCURS 5 TIMES PIC 9.                                 IF1094.2
            003700 01  TEMP                PIC S9(5)V9(5).                          IF1094.2
            003800 01  WS-DATE             PIC 9(7).                                IF1094.2
            003900*                                                                 IF1094.2
            004000**********************************************************        IF1094.2
            004100*                                                                 IF1094.2
            004200 01  TEST-RESULTS.                                                IF1094.2
            004300     02 FILLER                   PIC X      VALUE SPACE.          IF1094.2
            004400     02 FEATURE                  PIC X(20)  VALUE SPACE.          IF1094.2
            004500     02 FILLER                   PIC X      VALUE SPACE.          IF1094.2
            004600     02 P-OR-F                   PIC X(5)   VALUE SPACE.          IF1094.2
            004700     02 FILLER                   PIC X      VALUE SPACE.          IF1094.2
            004800     02  PAR-NAME.                                                IF1094.2
            004900       03 FILLER                 PIC X(19)  VALUE SPACE.          IF1094.2
            005000       03  PARDOT-X              PIC X      VALUE SPACE.          IF1094.2
            005100       03 DOTVALUE               PIC 99     VALUE ZERO.           IF1094.2
            005200     02 FILLER                   PIC X(8)   VALUE SPACE.          IF1094.2
            005300     02 RE-MARK                  PIC X(61).                       IF1094.2
            005400 01  TEST-COMPUTED.                                               IF1094.2
            005500     02 FILLER                   PIC X(30)  VALUE SPACE.          IF1094.2
            005600     02 FILLER                   PIC X(17)  VALUE                 IF1094.2
            005700            "       COMPUTED=".                                   IF1094.2
            005800     02 COMPUTED-X.                                               IF1094.2
            005900     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          IF1094.2
            006000     03 COMPUTED-N               REDEFINES COMPUTED-A             IF1094.2
            006100                                 PIC -9(9).9(9).                  IF1094.2
            006200     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         IF1094.2
            006300     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     IF1094.2
            006400     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     IF1094.2
            006500     03       CM-18V0 REDEFINES COMPUTED-A.                       IF1094.2
            006600         04 COMPUTED-18V0                    PIC -9(18).          IF1094.2
            006700         04 FILLER                           PIC X.               IF1094.2
            006800     03 FILLER PIC X(50) VALUE SPACE.                             IF1094.2
            006900 01  TEST-CORRECT.                                                IF1094.2
            007000     02 FILLER PIC X(30) VALUE SPACE.                             IF1094.2
            007100     02 FILLER PIC X(17) VALUE "       CORRECT =".                IF1094.2
            007200     02 CORRECT-X.                                                IF1094.2
            007300     03 CORRECT-A                  PIC X(20) VALUE SPACE.         IF1094.2
            007400     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      IF1094.2
            007500     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         IF1094.2
            007600     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     IF1094.2
            007700     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     IF1094.2
            007800     03      CR-18V0 REDEFINES CORRECT-A.                         IF1094.2
            007900         04 CORRECT-18V0                     PIC -9(18).          IF1094.2
            008000         04 FILLER                           PIC X.               IF1094.2
            008100     03 FILLER PIC X(2) VALUE SPACE.                              IF1094.2
            008200     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     IF1094.2
            008300 01  CCVS-C-1.                                                    IF1094.2
            008400     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PAIF1094.2
            008500-    "SS  PARAGRAPH-NAME                                          IF1094.2
            008600-    "       REMARKS".                                            IF1094.2
            008700     02 FILLER                     PIC X(20)    VALUE SPACE.      IF1094.2
            008800 01  CCVS-C-2.                                                    IF1094.2
            008900     02 FILLER                     PIC X        VALUE SPACE.      IF1094.2
            009000     02 FILLER                     PIC X(6)     VALUE "TESTED".   IF1094.2
            009100     02 FILLER                     PIC X(15)    VALUE SPACE.      IF1094.2
            009200     02 FILLER                     PIC X(4)     VALUE "FAIL".     IF1094.2
            009300     02 FILLER                     PIC X(94)    VALUE SPACE.      IF1094.2
            009400 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       IF1094.2
            009500 01  REC-CT                        PIC 99       VALUE ZERO.       IF1094.2
            009600 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       IF1094.2
            009700 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       IF1094.2
            009800 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       IF1094.2
            009900 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       IF1094.2
            010000 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       IF1094.2
            010100 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       IF1094.2
            010200 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      IF1094.2
            010300 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       IF1094.2
            010400 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     IF1094.2
            010500 01  CCVS-H-1.                                                    IF1094.2
            010600     02  FILLER                    PIC X(39)    VALUE SPACES.     IF1094.2
            010700     02  FILLER                    PIC X(42)    VALUE             IF1094.2
            010800     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 IF1094.2
            010900     02  FILLER                    PIC X(39)    VALUE SPACES.     IF1094.2
            011000 01  CCVS-H-2A.                                                   IF1094.2
            011100   02  FILLER                        PIC X(40)  VALUE SPACE.      IF1094.2
            011200   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  IF1094.2
            011300   02  FILLER                        PIC XXXX   VALUE             IF1094.2
            011400     "4.2 ".                                                      IF1094.2
            011500   02  FILLER                        PIC X(28)  VALUE             IF1094.2
            011600            " COPY - NOT FOR DISTRIBUTION".                       IF1094.2
            011700   02  FILLER                        PIC X(41)  VALUE SPACE.      IF1094.2
            011800                                                                  IF1094.2
            011900 01  CCVS-H-2B.                                                   IF1094.2
            012000   02  FILLER                        PIC X(15)  VALUE             IF1094.2
            012100            "TEST RESULT OF ".                                    IF1094.2
            012200   02  TEST-ID                       PIC X(9).                    IF1094.2
            012300   02  FILLER                        PIC X(4)   VALUE             IF1094.2
            012400            " IN ".                                               IF1094.2
            012500   02  FILLER                        PIC X(12)  VALUE             IF1094.2
            012600     " HIGH       ".                                              IF1094.2
            012700   02  FILLER                        PIC X(22)  VALUE             IF1094.2
            012800            " LEVEL VALIDATION FOR ".                             IF1094.2
            012900   02  FILLER                        PIC X(58)  VALUE             IF1094.2
            013000     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".IF1094.2
            013100 01  CCVS-H-3.                                                    IF1094.2
            013200     02  FILLER                      PIC X(34)  VALUE             IF1094.2
            013300            " FOR OFFICIAL USE ONLY    ".                         IF1094.2
            013400     02  FILLER                      PIC X(58)  VALUE             IF1094.2
            013500     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".IF1094.2
            013600     02  FILLER                      PIC X(28)  VALUE             IF1094.2
            013700            "  COPYRIGHT   1985 ".                                IF1094.2
            013800 01  CCVS-E-1.                                                    IF1094.2
            013900     02 FILLER                       PIC X(52)  VALUE SPACE.      IF1094.2
            014000     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              IF1094.2
            014100     02 ID-AGAIN                     PIC X(9).                    IF1094.2
            014200     02 FILLER                       PIC X(45)  VALUE SPACES.     IF1094.2
            014300 01  CCVS-E-2.                                                    IF1094.2
            014400     02  FILLER                      PIC X(31)  VALUE SPACE.      IF1094.2
            014500     02  FILLER                      PIC X(21)  VALUE SPACE.      IF1094.2
            014600     02 CCVS-E-2-2.                                               IF1094.2
            014700         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      IF1094.2
            014800         03 FILLER                   PIC X      VALUE SPACE.      IF1094.2
            014900         03 ENDER-DESC               PIC X(44)  VALUE             IF1094.2
            015000            "ERRORS ENCOUNTERED".                                 IF1094.2
            015100 01  CCVS-E-3.                                                    IF1094.2
            015200     02  FILLER                      PIC X(22)  VALUE             IF1094.2
            015300            " FOR OFFICIAL USE ONLY".                             IF1094.2
            015400     02  FILLER                      PIC X(12)  VALUE SPACE.      IF1094.2
            015500     02  FILLER                      PIC X(58)  VALUE             IF1094.2
            015600     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".IF1094.2
            015700     02  FILLER                      PIC X(13)  VALUE SPACE.      IF1094.2
            015800     02 FILLER                       PIC X(15)  VALUE             IF1094.2
            015900             " COPYRIGHT 1985".                                   IF1094.2
            016000 01  CCVS-E-4.                                                    IF1094.2
            016100     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      IF1094.2
            016200     02 FILLER                       PIC X(4)   VALUE " OF ".     IF1094.2
            016300     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      IF1094.2
            016400     02 FILLER                       PIC X(40)  VALUE             IF1094.2
            016500      "  TESTS WERE EXECUTED SUCCESSFULLY".                       IF1094.2
            016600 01  XXINFO.                                                      IF1094.2
            016700     02 FILLER                       PIC X(19)  VALUE             IF1094.2
            016800            "*** INFORMATION ***".                                IF1094.2
            016900     02 INFO-TEXT.                                                IF1094.2
            017000       04 FILLER                     PIC X(8)   VALUE SPACE.      IF1094.2
            017100       04 XXCOMPUTED                 PIC X(20).                   IF1094.2
            017200       04 FILLER                     PIC X(5)   VALUE SPACE.      IF1094.2
            017300       04 XXCORRECT                  PIC X(20).                   IF1094.2
            017400     02 INF-ANSI-REFERENCE           PIC X(48).                   IF1094.2
            017500 01  HYPHEN-LINE.                                                 IF1094.2
            017600     02 FILLER  PIC IS X VALUE IS SPACE.                          IF1094.2
            017700     02 FILLER  PIC IS X(65)    VALUE IS "************************IF1094.2
            017800-    "*****************************************".                 IF1094.2
            017900     02 FILLER  PIC IS X(54)    VALUE IS "************************IF1094.2
            018000-    "******************************".                            IF1094.2
            018100 01  CCVS-PGM-ID                     PIC X(9)   VALUE             IF1094.2
            018200     "IF109A".                                                    IF1094.2
            018300 PROCEDURE DIVISION.                                              IF1094.2
            018400 CCVS1 SECTION.                                                   IF1094.2
            018500 OPEN-FILES.                                                      IF1094.2
            018600     OPEN     OUTPUT PRINT-FILE.                                  IF1094.2
            018700     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   IF1094.2
            018800     MOVE    SPACE TO TEST-RESULTS.                               IF1094.2
            018900     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             IF1094.2
            019000     GO TO CCVS1-EXIT.                                            IF1094.2
            019100 CLOSE-FILES.                                                     IF1094.2
            019200     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   IF1094.2
            019300 TERMINATE-CCVS.                                                  IF1094.2
            019400     STOP     RUN.                                                IF1094.2
            019500 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         IF1094.2
            019600 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           IF1094.2
            019700 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          IF1094.2
            019800 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      IF1094.2
            019900     MOVE "****TEST DELETED****" TO RE-MARK.                      IF1094.2
            020000 PRINT-DETAIL.                                                    IF1094.2
            020100     IF REC-CT NOT EQUAL TO ZERO                                  IF1094.2
            020200             MOVE "." TO PARDOT-X                                 IF1094.2
            020300             MOVE REC-CT TO DOTVALUE.                             IF1094.2
            020400     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      IF1094.2
            020500     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               IF1094.2
            020600        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 IF1094.2
            020700          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 IF1094.2
            020800     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              IF1094.2
            020900     MOVE SPACE TO CORRECT-X.                                     IF1094.2
            021000     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         IF1094.2
            021100     MOVE     SPACE TO RE-MARK.                                   IF1094.2
            021200 HEAD-ROUTINE.                                                    IF1094.2
            021300     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  IF1094.2
            021400     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  IF1094.2
            021500     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  IF1094.2
            021600     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  IF1094.2
            021700 COLUMN-NAMES-ROUTINE.                                            IF1094.2
            021800     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           IF1094.2
            021900     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1094.2
            022000     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        IF1094.2
            022100 END-ROUTINE.                                                     IF1094.2
            022200     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.IF1094.2
            022300 END-RTN-EXIT.                                                    IF1094.2
            022400     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1094.2
            022500 END-ROUTINE-1.                                                   IF1094.2
            022600      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      IF1094.2
            022700      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               IF1094.2
            022800      ADD PASS-COUNTER TO ERROR-HOLD.                             IF1094.2
            022900      MOVE PASS-COUNTER TO CCVS-E-4-1.                            IF1094.2
            023000      MOVE ERROR-HOLD TO CCVS-E-4-2.                              IF1094.2
            023100      MOVE CCVS-E-4 TO CCVS-E-2-2.                                IF1094.2
            023200      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           IF1094.2
            023300  END-ROUTINE-12.                                                 IF1094.2
            023400      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        IF1094.2
            023500     IF       ERROR-COUNTER IS EQUAL TO ZERO                      IF1094.2
            023600         MOVE "NO " TO ERROR-TOTAL                                IF1094.2
            023700         ELSE                                                     IF1094.2
            023800         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       IF1094.2
            023900     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           IF1094.2
            024000     PERFORM WRITE-LINE.                                          IF1094.2
            024100 END-ROUTINE-13.                                                  IF1094.2
            024200     IF DELETE-COUNTER IS EQUAL TO ZERO                           IF1094.2
            024300         MOVE "NO " TO ERROR-TOTAL  ELSE                          IF1094.2
            024400         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      IF1094.2
            024500     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   IF1094.2
            024600     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           IF1094.2
            024700      IF   INSPECT-COUNTER EQUAL TO ZERO                          IF1094.2
            024800          MOVE "NO " TO ERROR-TOTAL                               IF1094.2
            024900      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   IF1094.2
            025000      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            IF1094.2
            025100      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          IF1094.2
            025200     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           IF1094.2
            025300 WRITE-LINE.                                                      IF1094.2
            025400     ADD 1 TO RECORD-COUNT.                                       IF1094.2
            025500*    IF RECORD-COUNT GREATER 42                                   IF1094.2
            025600*        MOVE DUMMY-RECORD TO DUMMY-HOLD                          IF1094.2
            025700*        MOVE SPACE TO DUMMY-RECORD                               IF1094.2
            025800*        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  IF1094.2
            025900*        MOVE CCVS-H-1  TO DUMMY-RECORD  PERFORM WRT-LN 2 TIMES   IF1094.2
            026000*        MOVE CCVS-H-2A TO DUMMY-RECORD  PERFORM WRT-LN 2 TIMES   IF1094.2
            026100*        MOVE CCVS-H-2B TO DUMMY-RECORD  PERFORM WRT-LN 3 TIMES   IF1094.2
            026200*        MOVE CCVS-H-3  TO DUMMY-RECORD  PERFORM WRT-LN 3 TIMES   IF1094.2
            026300*        MOVE CCVS-C-1  TO DUMMY-RECORD  PERFORM WRT-LN           IF1094.2
            026400*        MOVE CCVS-C-2  TO DUMMY-RECORD  PERFORM WRT-LN           IF1094.2
            026500*        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          IF1094.2
            026600*        MOVE DUMMY-HOLD TO DUMMY-RECORD                          IF1094.2
            026700*        MOVE ZERO TO RECORD-COUNT.                               IF1094.2
            026800     PERFORM WRT-LN.                                              IF1094.2
            026900 WRT-LN.                                                          IF1094.2
            027000     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               IF1094.2
            027100     MOVE SPACE TO DUMMY-RECORD.                                  IF1094.2
            027200 BLANK-LINE-PRINT.                                                IF1094.2
            027300     PERFORM WRT-LN.                                              IF1094.2
            027400 FAIL-ROUTINE.                                                    IF1094.2
            027500     IF     COMPUTED-X NOT EQUAL TO SPACE                         IF1094.2
            027600            GO TO FAIL-ROUTINE-WRITE.                             IF1094.2
            027700     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.IF1094.2
            027800     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 IF1094.2
            027900     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   IF1094.2
            028000     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1094.2
            028100     MOVE   SPACES TO INF-ANSI-REFERENCE.                         IF1094.2
            028200     GO TO  FAIL-ROUTINE-EX.                                      IF1094.2
            028300 FAIL-ROUTINE-WRITE.                                              IF1094.2
            028400     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         IF1094.2
            028500     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 IF1094.2
            028600     MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. IF1094.2
            028700     MOVE   SPACES TO COR-ANSI-REFERENCE.                         IF1094.2
            028800 FAIL-ROUTINE-EX. EXIT.                                           IF1094.2
            028900 BAIL-OUT.                                                        IF1094.2
            029000     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   IF1094.2
            029100     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           IF1094.2
            029200 BAIL-OUT-WRITE.                                                  IF1094.2
            029300     MOVE CORRECT-A TO XXCORRECT.                                 IF1094.2
            029400     MOVE COMPUTED-A TO XXCOMPUTED.                               IF1094.2
            029500     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 IF1094.2
            029600     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1094.2
            029700     MOVE   SPACES TO INF-ANSI-REFERENCE.                         IF1094.2
            029800 BAIL-OUT-EX. EXIT.                                               IF1094.2
            029900 CCVS1-EXIT.                                                      IF1094.2
            030000     EXIT.                                                        IF1094.2
            030100********************************************************          IF1094.2
            030200*                                                      *          IF1094.2
            030300*    Intrinsic Function Test  IF109A - DAY-OF-INTEGER  *          IF1094.2
            030400*                                                      *          IF1094.2
            030500********************************************************          IF1094.2
            030600 SECT-IF109A SECTION.                                             IF1094.2
            030700 F-DAYOFINT-INFO.                                                 IF1094.2
            030800     MOVE     "See ref. A-42 2.13" TO ANSI-REFERENCE.             IF1094.2
            030900     MOVE     "DAY-OF-INTEGER"     TO FEATURE.                    IF1094.2
            031000*****************TEST (a) ******************************          IF1094.2
            031100 F-DAYOFINT-01.                                                   IF1094.2
            031200     MOVE ZERO TO WS-DATE.                                        IF1094.2
            031300 F-DAYOFINT-TEST-01.                                              IF1094.2
            031400     COMPUTE WS-DATE = FUNCTION DAY-OF-INTEGER(1).                IF1094.2
            031500     IF WS-DATE = 1601001 THEN                                    IF1094.2
            031600                        PERFORM PASS                              IF1094.2
            031700     ELSE                                                         IF1094.2
            031800                        MOVE  1601001  TO CORRECT-N               IF1094.2
            031900                        MOVE WS-DATE TO COMPUTED-N                IF1094.2
            032000                        PERFORM FAIL.                             IF1094.2
            032100     GO TO F-DAYOFINT-WRITE-01.                                   IF1094.2
            032200 F-DAYOFINT-DELETE-01.                                            IF1094.2
            032300     PERFORM  DE-LETE.                                            IF1094.2
            032400     GO TO    F-DAYOFINT-WRITE-01.                                IF1094.2
            032500 F-DAYOFINT-WRITE-01.                                             IF1094.2
            032600     MOVE "F-DAYOFINT-01" TO PAR-NAME.                            IF1094.2
            032700     PERFORM  PRINT-DETAIL.                                       IF1094.2
            032800*****************TEST (b) ******************************          IF1094.2
            032900 F-DAYOFINT-TEST-02.                                              IF1094.2
            033000     EVALUATE FUNCTION DAY-OF-INTEGER(A)                          IF1094.2
            033100     WHEN     1602035                                             IF1094.2
            033200                    PERFORM PASS                                  IF1094.2
            033300                    GO TO F-DAYOFINT-WRITE-02.                    IF1094.2
            033400     PERFORM FAIL.                                                IF1094.2
            033500     GO TO F-DAYOFINT-WRITE-02.                                   IF1094.2
            033600 F-DAYOFINT-DELETE-02.                                            IF1094.2
            033700     PERFORM  DE-LETE.                                            IF1094.2
            033800     GO TO    F-DAYOFINT-WRITE-02.                                IF1094.2
            033900 F-DAYOFINT-WRITE-02.                                             IF1094.2
            034000     MOVE "F-DAYOFINT-02" TO PAR-NAME.                            IF1094.2
            034100     PERFORM  PRINT-DETAIL.                                       IF1094.2
            034200*****************TEST (c) ******************************          IF1094.2
            034300 F-DAYOFINT-TEST-03.                                              IF1094.2
            034400     IF FUNCTION DAY-OF-INTEGER(IND(1)) = 1601004 THEN            IF1094.2
            034500                        PERFORM PASS                              IF1094.2
            034600     ELSE                                                         IF1094.2
            034700                        PERFORM FAIL.                             IF1094.2
            034800     GO TO F-DAYOFINT-WRITE-03.                                   IF1094.2
            034900 F-DAYOFINT-DELETE-03.                                            IF1094.2
            035000     PERFORM  DE-LETE.                                            IF1094.2
            035100     GO TO    F-DAYOFINT-WRITE-03.                                IF1094.2
            035200 F-DAYOFINT-WRITE-03.                                             IF1094.2
            035300     MOVE "F-DAYOFINT-03" TO PAR-NAME.                            IF1094.2
            035400     PERFORM  PRINT-DETAIL.                                       IF1094.2
            035500*****************TEST (d) ******************************          IF1094.2
            035600 F-DAYOFINT-04.                                                   IF1094.2
            035700     MOVE ZERO TO WS-DATE.                                        IF1094.2
            035800 F-DAYOFINT-TEST-04.                                              IF1094.2
            035900     COMPUTE WS-DATE = FUNCTION DAY-OF-INTEGER(IND(D)).           IF1094.2
            036000     IF WS-DATE = 1601004 THEN                                    IF1094.2
            036100                        PERFORM PASS                              IF1094.2
            036200     ELSE                                                         IF1094.2
            036300                        MOVE  1601004  TO CORRECT-N               IF1094.2
            036400                        MOVE WS-DATE TO COMPUTED-N                IF1094.2
            036500                        PERFORM FAIL.                             IF1094.2
            036600     GO TO F-DAYOFINT-WRITE-04.                                   IF1094.2
            036700 F-DAYOFINT-DELETE-04.                                            IF1094.2
            036800     PERFORM  DE-LETE.                                            IF1094.2
            036900     GO TO    F-DAYOFINT-WRITE-04.                                IF1094.2
            037000 F-DAYOFINT-WRITE-04.                                             IF1094.2
            037100     MOVE "F-DAYOFINT-04" TO PAR-NAME.                            IF1094.2
            037200     PERFORM  PRINT-DETAIL.                                       IF1094.2
            037300*****************TEST (e) ******************************          IF1094.2
            037400 F-DAYOFINT-05.                                                   IF1094.2
            037500     MOVE ZERO TO WS-DATE.                                        IF1094.2
            037600 F-DAYOFINT-TEST-05.                                              IF1094.2
            037700     COMPUTE WS-DATE = FUNCTION DAY-OF-INTEGER(C).                IF1094.2
            037800     IF WS-DATE = 1601365 THEN                                    IF1094.2
            037900                        PERFORM PASS                              IF1094.2
            038000     ELSE                                                         IF1094.2
            038100                        MOVE  1601365  TO CORRECT-N               IF1094.2
            038200                        MOVE WS-DATE TO COMPUTED-N                IF1094.2
            038300                        PERFORM FAIL.                             IF1094.2
            038400     GO TO F-DAYOFINT-WRITE-05.                                   IF1094.2
            038500 F-DAYOFINT-DELETE-05.                                            IF1094.2
            038600     PERFORM  DE-LETE.                                            IF1094.2
            038700     GO TO    F-DAYOFINT-WRITE-05.                                IF1094.2
            038800 F-DAYOFINT-WRITE-05.                                             IF1094.2
            038900     MOVE "F-DAYOFINT-05" TO PAR-NAME.                            IF1094.2
            039000     PERFORM  PRINT-DETAIL.                                       IF1094.2
            039100*****************TEST (f) ******************************          IF1094.2
            039200 F-DAYOFINT-06.                                                   IF1094.2
            039300     MOVE ZERO TO WS-DATE.                                        IF1094.2
            039400 F-DAYOFINT-TEST-06.                                              IF1094.2
            039500     COMPUTE WS-DATE = FUNCTION DAY-OF-INTEGER(D) + 10.           IF1094.2
            039600     IF WS-DATE = 1601011 THEN                                    IF1094.2
            039700                        PERFORM PASS                              IF1094.2
            039800     ELSE                                                         IF1094.2
            039900                        MOVE  1601011  TO CORRECT-N               IF1094.2
            040000                        MOVE WS-DATE TO COMPUTED-N                IF1094.2
            040100                        PERFORM FAIL.                             IF1094.2
            040200     GO TO F-DAYOFINT-WRITE-06.                                   IF1094.2
            040300 F-DAYOFINT-DELETE-06.                                            IF1094.2
            040400     PERFORM  DE-LETE.                                            IF1094.2
            040500     GO TO    F-DAYOFINT-WRITE-06.                                IF1094.2
            040600 F-DAYOFINT-WRITE-06.                                             IF1094.2
            040700     MOVE "F-DAYOFINT-06" TO PAR-NAME.                            IF1094.2
            040800     PERFORM  PRINT-DETAIL.                                       IF1094.2
            040900*****************TEST (g) ******************************          IF1094.2
            041000 F-DAYOFINT-07.                                                   IF1094.2
            041100     MOVE ZERO TO WS-DATE.                                        IF1094.2
            041200 F-DAYOFINT-TEST-07.                                              IF1094.2
            041300     COMPUTE WS-DATE = FUNCTION DAY-OF-INTEGER(D) +               IF1094.2
            041400                       FUNCTION DAY-OF-INTEGER(D).                IF1094.2
            041500     IF WS-DATE = 3202002 THEN                                    IF1094.2
            041600                        PERFORM PASS                              IF1094.2
            041700     ELSE                                                         IF1094.2
            041800                        MOVE  3202002  TO CORRECT-N               IF1094.2
            041900                        MOVE WS-DATE TO COMPUTED-N                IF1094.2
            042000                        PERFORM FAIL.                             IF1094.2
            042100     GO TO F-DAYOFINT-WRITE-07.                                   IF1094.2
            042200 F-DAYOFINT-DELETE-07.                                            IF1094.2
            042300     PERFORM  DE-LETE.                                            IF1094.2
            042400     GO TO    F-DAYOFINT-WRITE-07.                                IF1094.2
            042500 F-DAYOFINT-WRITE-07.                                             IF1094.2
            042600     MOVE "F-DAYOFINT-07" TO PAR-NAME.                            IF1094.2
            042700     PERFORM  PRINT-DETAIL.                                       IF1094.2
            042800***************** SPECIAL TEST 1 ***********************          IF1094.2
            042900 F-DAYOFINT-08.                                                   IF1094.2
            043000     MOVE 1 TO ARG1.                                              IF1094.2
            043100     PERFORM F-DAYOFINT-TEST-08                                   IF1094.2
            043200             UNTIL FUNCTION DAY-OF-INTEGER(ARG1) > 1601010.       IF1094.2
            043300     IF ARG1 = 11 THEN                                            IF1094.2
            043400                            PERFORM PASS                          IF1094.2
            043500     ELSE                                                         IF1094.2
            043600                            PERFORM FAIL.                         IF1094.2
            043700     GO TO F-DAYOFINT-WRITE-08.                                   IF1094.2
            043800*                                                                 IF1094.2
            043900 F-DAYOFINT-TEST-08.                                              IF1094.2
            044000     COMPUTE ARG1 = ARG1 + 1.                                     IF1094.2
            044100*                                                                 IF1094.2
            044200 F-DAYOFINT-DELETE-08.                                            IF1094.2
            044300     PERFORM  DE-LETE.                                            IF1094.2
            044400     GO TO    F-DAYOFINT-WRITE-08.                                IF1094.2
            044500 F-DAYOFINT-WRITE-08.                                             IF1094.2
            044600     MOVE "F-DAYOFINT-08" TO PAR-NAME.                            IF1094.2
            044700     PERFORM  PRINT-DETAIL.                                       IF1094.2
            044800*******************END OF TESTS**************************         IF1094.2
            044900 CCVS-EXIT SECTION.                                               IF1094.2
            045000 CCVS-999999.                                                     IF1094.2
            045100     GO TO CLOSE-FILES.                                           IF1094.2
                  *END-OF,IF109A                                                                  
        """)
    )

    @Test
    fun if1104_2() = rewriteRun(
        cobol("""
                  *HEADER,COBOL,IF110A                                                            
            000100 IDENTIFICATION DIVISION.                                         IF1104.2
            000200 PROGRAM-ID.                                                      IF1104.2
            000300     IF110A.                                                      IF1104.2
            000400                                                                  IF1104.2
            000500***********************************************************       IF1104.2
            000600*                                                         *       IF1104.2
            000700* This program forms part of the CCVS85 COBOL Test Suite. *       IF1104.2
            000800* It contains tests for the Intrinsic Function            *       IF1104.2
            000900* FACTORIAL.                                              *       IF1104.2
            001000*                                                         *       IF1104.2
            001100***********************************************************       IF1104.2
            001200 ENVIRONMENT DIVISION.                                            IF1104.2
            001300 CONFIGURATION SECTION.                                           IF1104.2
            001400 SOURCE-COMPUTER.                                                 IF1104.2
            001500     XXXXX082.                                                    IF1104.2
            001600 OBJECT-COMPUTER.                                                 IF1104.2
            001700     XXXXX083.                                                    IF1104.2
            001800 INPUT-OUTPUT SECTION.                                            IF1104.2
            001900 FILE-CONTROL.                                                    IF1104.2
            002000     SELECT PRINT-FILE ASSIGN TO                                  IF1104.2
            002100     XXXXX055.                                                    IF1104.2
            002200 DATA DIVISION.                                                   IF1104.2
            002300 FILE SECTION.                                                    IF1104.2
            002400 FD  PRINT-FILE.                                                  IF1104.2
            002500 01  PRINT-REC PICTURE X(120).                                    IF1104.2
            002600 01  DUMMY-RECORD PICTURE X(120).                                 IF1104.2
            002700 WORKING-STORAGE SECTION.                                         IF1104.2
            002800***********************************************************       IF1104.2
            002900* Variables specific to the Intrinsic Function Test IF110A*       IF1104.2
            003000***********************************************************       IF1104.2
            003100 01  A                   PIC S9(10)          VALUE 5.             IF1104.2
            003200 01  B                   PIC S9(10)          VALUE 7.             IF1104.2
            003300 01  ARG1                PIC S9(10)          VALUE 1.             IF1104.2
            003400 01  ARR                                     VALUE "40537".       IF1104.2
            003500     02  IND OCCURS 5 TIMES PIC 9.                                IF1104.2
            003600 01  TEMP                PIC S9(5)V9(5).                          IF1104.2
            003700 01  WS-NUM              PIC S9(5)V9(6).                          IF1104.2
            003800 01  MIN-RANGE           PIC S9(5)V9(7).                          IF1104.2
            003900 01  MAX-RANGE           PIC S9(5)V9(7).                          IF1104.2
            004000*                                                                 IF1104.2
            004100**********************************************************        IF1104.2
            004200*                                                                 IF1104.2
            004300 01  TEST-RESULTS.                                                IF1104.2
            004400     02 FILLER                   PIC X      VALUE SPACE.          IF1104.2
            004500     02 FEATURE                  PIC X(20)  VALUE SPACE.          IF1104.2
            004600     02 FILLER                   PIC X      VALUE SPACE.          IF1104.2
            004700     02 P-OR-F                   PIC X(5)   VALUE SPACE.          IF1104.2
            004800     02 FILLER                   PIC X      VALUE SPACE.          IF1104.2
            004900     02  PAR-NAME.                                                IF1104.2
            005000       03 FILLER                 PIC X(19)  VALUE SPACE.          IF1104.2
            005100       03  PARDOT-X              PIC X      VALUE SPACE.          IF1104.2
            005200       03 DOTVALUE               PIC 99     VALUE ZERO.           IF1104.2
            005300     02 FILLER                   PIC X(8)   VALUE SPACE.          IF1104.2
            005400     02 RE-MARK                  PIC X(61).                       IF1104.2
            005500 01  TEST-COMPUTED.                                               IF1104.2
            005600     02 FILLER                   PIC X(30)  VALUE SPACE.          IF1104.2
            005700     02 FILLER                   PIC X(17)  VALUE                 IF1104.2
            005800            "       COMPUTED=".                                   IF1104.2
            005900     02 COMPUTED-X.                                               IF1104.2
            006000     03 COMPUTED-A               PIC X(20)  VALUE SPACE.          IF1104.2
            006100     03 COMPUTED-N               REDEFINES COMPUTED-A             IF1104.2
            006200                                 PIC -9(9).9(9).                  IF1104.2
            006300     03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         IF1104.2
            006400     03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     IF1104.2
            006500     03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     IF1104.2
            006600     03       CM-18V0 REDEFINES COMPUTED-A.                       IF1104.2
            006700         04 COMPUTED-18V0                    PIC -9(18).          IF1104.2
            006800         04 FILLER                           PIC X.               IF1104.2
            006900     03 FILLER PIC X(50) VALUE SPACE.                             IF1104.2
            007000 01  TEST-CORRECT.                                                IF1104.2
            007100     02 FILLER PIC X(30) VALUE SPACE.                             IF1104.2
            007200     02 FILLER PIC X(17) VALUE "       CORRECT =".                IF1104.2
            007300     02 CORRECT-X.                                                IF1104.2
            007400     03 CORRECT-A                  PIC X(20) VALUE SPACE.         IF1104.2
            007500     03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      IF1104.2
            007600     03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         IF1104.2
            007700     03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     IF1104.2
            007800     03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     IF1104.2
            007900     03      CR-18V0 REDEFINES CORRECT-A.                         IF1104.2
            008000         04 CORRECT-18V0                     PIC -9(18).          IF1104.2
            008100         04 FILLER                           PIC X.               IF1104.2
            008200     03 FILLER PIC X(2) VALUE SPACE.                              IF1104.2
            008300     03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     IF1104.2
            008400 01  TEST-CORRECT-MIN.                                            IF1104.2
            008500     02 FILLER PIC X(30) VALUE SPACE.                             IF1104.2
            008600     02 FILLER PIC X(17) VALUE "     MIN VALUE =".                IF1104.2
            008700     02 CORRECTMI-X.                                              IF1104.2
            008800     03 CORRECTMI-A                 PIC X(20) VALUE SPACE.        IF1104.2
            008900     03 CORRECT-MIN    REDEFINES CORRECTMI-A     PIC -9(9).9(9).  IF1104.2
            009000     03 CORRECTMI-0V18 REDEFINES CORRECTMI-A     PIC -.9(18).     IF1104.2
            009100     03 CORRECTMI-4V14 REDEFINES CORRECTMI-A     PIC -9(4).9(14). IF1104.2
            009200     03 CORRECTMI-14V4 REDEFINES CORRECTMI-A     PIC -9(14).9(4). IF1104.2
            009300     03      CR-18V0 REDEFINES CORRECTMI-A.                       IF1104.2
            009400         04 CORRECTMI-18V0                     PIC -9(18).        IF1104.2
            009500         04 FILLER                           PIC X.               IF1104.2
            009600     03 FILLER PIC X(2) VALUE SPACE.                              IF1104.2
            009700     03 FILLER                           PIC X(48) VALUE SPACE.   IF1104.2
            009800 01  TEST-CORRECT-MAX.                                            IF1104.2
            009900     02 FILLER PIC X(30) VALUE SPACE.                             IF1104.2
            010000     02 FILLER PIC X(17) VALUE "     MAX VALUE =".                IF1104.2
            010100     02 CORRECTMA-X.                                              IF1104.2
            010200     03 CORRECTMA-A                  PIC X(20) VALUE SPACE.       IF1104.2
            010300     03 CORRECT-MAX    REDEFINES CORRECTMA-A     PIC -9(9).9(9).  IF1104.2
            010400     03 CORRECTMA-0V18 REDEFINES CORRECTMA-A     PIC -.9(18).     IF1104.2
            010500     03 CORRECTMA-4V14 REDEFINES CORRECTMA-A     PIC -9(4).9(14). IF1104.2
            010600     03 CORRECTMA-14V4 REDEFINES CORRECTMA-A     PIC -9(14).9(4). IF1104.2
            010700     03      CR-18V0 REDEFINES CORRECTMA-A.                       IF1104.2
            010800         04 CORRECTMA-18V0                     PIC -9(18).        IF1104.2
            010900         04 FILLER                           PIC X.               IF1104.2
            011000     03 FILLER PIC X(2) VALUE SPACE.                              IF1104.2
            011100     03 CORMA-ANSI-REFERENCE             PIC X(48) VALUE SPACE.   IF1104.2
            011200 01  CCVS-C-1.                                                    IF1104.2
            011300     02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PAIF1104.2
            011400-    "SS  PARAGRAPH-NAME                                          IF1104.2
            011500-    "       REMARKS".                                            IF1104.2
            011600     02 FILLER                     PIC X(20)    VALUE SPACE.      IF1104.2
            011700 01  CCVS-C-2.                                                    IF1104.2
            011800     02 FILLER                     PIC X        VALUE SPACE.      IF1104.2
            011900     02 FILLER                     PIC X(6)     VALUE "TESTED".   IF1104.2
            012000     02 FILLER                     PIC X(15)    VALUE SPACE.      IF1104.2
            012100     02 FILLER                     PIC X(4)     VALUE "FAIL".     IF1104.2
            012200     02 FILLER                     PIC X(94)    VALUE SPACE.      IF1104.2
            012300 01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       IF1104.2
            012400 01  REC-CT                        PIC 99       VALUE ZERO.       IF1104.2
            012500 01  DELETE-COUNTER                PIC 999      VALUE ZERO.       IF1104.2
            012600 01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       IF1104.2
            012700 01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       IF1104.2
            012800 01  PASS-COUNTER                  PIC 999      VALUE ZERO.       IF1104.2
            012900 01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       IF1104.2
            013000 01  ERROR-HOLD                    PIC 999      VALUE ZERO.       IF1104.2
            013100 01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      IF1104.2
            013200 01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       IF1104.2
            013300 01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     IF1104.2
            013400 01  CCVS-H-1.                                                    IF1104.2
            013500     02  FILLER                    PIC X(39)    VALUE SPACES.     IF1104.2
            013600     02  FILLER                    PIC X(42)    VALUE             IF1104.2
            013700     "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 IF1104.2
            013800     02  FILLER                    PIC X(39)    VALUE SPACES.     IF1104.2
            013900 01  CCVS-H-2A.                                                   IF1104.2
            014000   02  FILLER                        PIC X(40)  VALUE SPACE.      IF1104.2
            014100   02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  IF1104.2
            014200   02  FILLER                        PIC XXXX   VALUE             IF1104.2
            014300     "4.2 ".                                                      IF1104.2
            014400   02  FILLER                        PIC X(28)  VALUE             IF1104.2
            014500            " COPY - NOT FOR DISTRIBUTION".                       IF1104.2
            014600   02  FILLER                        PIC X(41)  VALUE SPACE.      IF1104.2
            014700                                                                  IF1104.2
            014800 01  CCVS-H-2B.                                                   IF1104.2
            014900   02  FILLER                        PIC X(15)  VALUE             IF1104.2
            015000            "TEST RESULT OF ".                                    IF1104.2
            015100   02  TEST-ID                       PIC X(9).                    IF1104.2
            015200   02  FILLER                        PIC X(4)   VALUE             IF1104.2
            015300            " IN ".                                               IF1104.2
            015400   02  FILLER                        PIC X(12)  VALUE             IF1104.2
            015500     " HIGH       ".                                              IF1104.2
            015600   02  FILLER                        PIC X(22)  VALUE             IF1104.2
            015700            " LEVEL VALIDATION FOR ".                             IF1104.2
            015800   02  FILLER                        PIC X(58)  VALUE             IF1104.2
            015900     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".IF1104.2
            016000 01  CCVS-H-3.                                                    IF1104.2
            016100     02  FILLER                      PIC X(34)  VALUE             IF1104.2
            016200            " FOR OFFICIAL USE ONLY    ".                         IF1104.2
            016300     02  FILLER                      PIC X(58)  VALUE             IF1104.2
            016400     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".IF1104.2
            016500     02  FILLER                      PIC X(28)  VALUE             IF1104.2
            016600            "  COPYRIGHT   1985 ".                                IF1104.2
            016700 01  CCVS-E-1.                                                    IF1104.2
            016800     02 FILLER                       PIC X(52)  VALUE SPACE.      IF1104.2
            016900     02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              IF1104.2
            017000     02 ID-AGAIN                     PIC X(9).                    IF1104.2
            017100     02 FILLER                       PIC X(45)  VALUE SPACES.     IF1104.2
            017200 01  CCVS-E-2.                                                    IF1104.2
            017300     02  FILLER                      PIC X(31)  VALUE SPACE.      IF1104.2
            017400     02  FILLER                      PIC X(21)  VALUE SPACE.      IF1104.2
            017500     02 CCVS-E-2-2.                                               IF1104.2
            017600         03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      IF1104.2
            017700         03 FILLER                   PIC X      VALUE SPACE.      IF1104.2
            017800         03 ENDER-DESC               PIC X(44)  VALUE             IF1104.2
            017900            "ERRORS ENCOUNTERED".                                 IF1104.2
            018000 01  CCVS-E-3.                                                    IF1104.2
            018100     02  FILLER                      PIC X(22)  VALUE             IF1104.2
            018200            " FOR OFFICIAL USE ONLY".                             IF1104.2
            018300     02  FILLER                      PIC X(12)  VALUE SPACE.      IF1104.2
            018400     02  FILLER                      PIC X(58)  VALUE             IF1104.2
            018500     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".IF1104.2
            018600     02  FILLER                      PIC X(13)  VALUE SPACE.      IF1104.2
            018700     02 FILLER                       PIC X(15)  VALUE             IF1104.2
            018800             " COPYRIGHT 1985".                                   IF1104.2
            018900 01  CCVS-E-4.                                                    IF1104.2
            019000     02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      IF1104.2
            019100     02 FILLER                       PIC X(4)   VALUE " OF ".     IF1104.2
            019200     02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      IF1104.2
            019300     02 FILLER                       PIC X(40)  VALUE             IF1104.2
            019400      "  TESTS WERE EXECUTED SUCCESSFULLY".                       IF1104.2
            019500 01  XXINFO.                                                      IF1104.2
            019600     02 FILLER                       PIC X(19)  VALUE             IF1104.2
            019700            "*** INFORMATION ***".                                IF1104.2
            019800     02 INFO-TEXT.                                                IF1104.2
            019900       04 FILLER                     PIC X(8)   VALUE SPACE.      IF1104.2
            020000       04 XXCOMPUTED                 PIC X(20).                   IF1104.2
            020100       04 FILLER                     PIC X(5)   VALUE SPACE.      IF1104.2
            020200       04 XXCORRECT                  PIC X(20).                   IF1104.2
            020300     02 INF-ANSI-REFERENCE           PIC X(48).                   IF1104.2
            020400 01  HYPHEN-LINE.                                                 IF1104.2
            020500     02 FILLER  PIC IS X VALUE IS SPACE.                          IF1104.2
            020600     02 FILLER  PIC IS X(65)    VALUE IS "************************IF1104.2
            020700-    "*****************************************".                 IF1104.2
            020800     02 FILLER  PIC IS X(54)    VALUE IS "************************IF1104.2
            020900-    "******************************".                            IF1104.2
            021000 01  CCVS-PGM-ID                     PIC X(9)   VALUE             IF1104.2
            021100     "IF110A".                                                    IF1104.2
            021200 PROCEDURE DIVISION.                                              IF1104.2
            021300 CCVS1 SECTION.                                                   IF1104.2
            021400 OPEN-FILES.                                                      IF1104.2
            021500     OPEN     OUTPUT PRINT-FILE.                                  IF1104.2
            021600     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   IF1104.2
            021700     MOVE    SPACE TO TEST-RESULTS.                               IF1104.2
            021800     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             IF1104.2
            021900     GO TO CCVS1-EXIT.                                            IF1104.2
            022000 CLOSE-FILES.                                                     IF1104.2
            022100     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   IF1104.2
            022200 TERMINATE-CCVS.                                                  IF1104.2
            022300     STOP     RUN.                                                IF1104.2
            022400 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         IF1104.2
            022500 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           IF1104.2
            022600 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          IF1104.2
            022700 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      IF1104.2
            022800     MOVE "****TEST DELETED****" TO RE-MARK.                      IF1104.2
            022900 PRINT-DETAIL.                                                    IF1104.2
            023000     IF REC-CT NOT EQUAL TO ZERO                                  IF1104.2
            023100             MOVE "." TO PARDOT-X                                 IF1104.2
            023200             MOVE REC-CT TO DOTVALUE.                             IF1104.2
            023300     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      IF1104.2
            023400     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               IF1104.2
            023500        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 IF1104.2
            023600          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 IF1104.2
            023700     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              IF1104.2
            023800     MOVE SPACE TO CORRECT-X.                                     IF1104.2
            023900     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         IF1104.2
            024000     MOVE     SPACE TO RE-MARK.                                   IF1104.2
            024100 HEAD-ROUTINE.                                                    IF1104.2
            024200     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  IF1104.2
            024300     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  IF1104.2
            024400     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  IF1104.2
            024500     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  IF1104.2
            024600 COLUMN-NAMES-ROUTINE.                                            IF1104.2
            024700     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           IF1104.2
            024800     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1104.2
            024900     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        IF1104.2
            025000 END-ROUTINE.                                                     IF1104.2
            025100     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.IF1104.2
            025200 END-RTN-EXIT.                                                    IF1104.2
            025300     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1104.2
            025400 END-ROUTINE-1.                                                   IF1104.2
            025500      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      IF1104.2
            025600      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               IF1104.2
            025700      ADD PASS-COUNTER TO ERROR-HOLD.                             IF1104.2
            025800      MOVE PASS-COUNTER TO CCVS-E-4-1.                            IF1104.2
            025900      MOVE ERROR-HOLD TO CCVS-E-4-2.                              IF1104.2
            026000      MOVE CCVS-E-4 TO CCVS-E-2-2.                                IF1104.2
            026100      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           IF1104.2
            026200  END-ROUTINE-12.                                                 IF1104.2
            026300      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        IF1104.2
            026400     IF       ERROR-COUNTER IS EQUAL TO ZERO                      IF1104.2
            026500         MOVE "NO " TO ERROR-TOTAL                                IF1104.2
            026600         ELSE                                                     IF1104.2
            026700         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       IF1104.2
            026800     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           IF1104.2
            026900     PERFORM WRITE-LINE.                                          IF1104.2
            027000 END-ROUTINE-13.                                                  IF1104.2
            027100     IF DELETE-COUNTER IS EQUAL TO ZERO                           IF1104.2
            027200         MOVE "NO " TO ERROR-TOTAL  ELSE                          IF1104.2
            027300         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      IF1104.2
            027400     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   IF1104.2
            027500     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           IF1104.2
            027600      IF   INSPECT-COUNTER EQUAL TO ZERO                          IF1104.2
            027700          MOVE "NO " TO ERROR-TOTAL                               IF1104.2
            027800      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   IF1104.2
            027900      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            IF1104.2
            028000      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          IF1104.2
            028100     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           IF1104.2
            028200 WRITE-LINE.                                                      IF1104.2
            028300     ADD 1 TO RECORD-COUNT.                                       IF1104.2
            028400*    IF RECORD-COUNT GREATER 42                                   IF1104.2
            028500*        MOVE DUMMY-RECORD TO DUMMY-HOLD                          IF1104.2
            028600*        MOVE SPACE TO DUMMY-RECORD                               IF1104.2
            028700*        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  IF1104.2
            028800*        MOVE CCVS-H-1  TO DUMMY-RECORD  PERFORM WRT-LN 2 TIMES   IF1104.2
            028900*        MOVE CCVS-H-2A TO DUMMY-RECORD  PERFORM WRT-LN 2 TIMES   IF1104.2
            029000*        MOVE CCVS-H-2B TO DUMMY-RECORD  PERFORM WRT-LN 3 TIMES   IF1104.2
            029100*        MOVE CCVS-H-3  TO DUMMY-RECORD  PERFORM WRT-LN 3 TIMES   IF1104.2
            029200*        MOVE CCVS-C-1  TO DUMMY-RECORD  PERFORM WRT-LN           IF1104.2
            029300*        MOVE CCVS-C-2  TO DUMMY-RECORD  PERFORM WRT-LN           IF1104.2
            029400*        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          IF1104.2
            029500*        MOVE DUMMY-HOLD TO DUMMY-RECORD                          IF1104.2
            029600*        MOVE ZERO TO RECORD-COUNT.                               IF1104.2
            029700     PERFORM WRT-LN.                                              IF1104.2
            029800 WRT-LN.                                                          IF1104.2
            029900     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               IF1104.2
            030000     MOVE SPACE TO DUMMY-RECORD.                                  IF1104.2
            030100 BLANK-LINE-PRINT.                                                IF1104.2
            030200     PERFORM WRT-LN.                                              IF1104.2
            030300 FAIL-ROUTINE.                                                    IF1104.2
            030400     IF     COMPUTED-X NOT EQUAL TO SPACE                         IF1104.2
            030500            GO TO FAIL-ROUTINE-WRITE.                             IF1104.2
            030600     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.IF1104.2
            030700     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 IF1104.2
            030800     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   IF1104.2
            030900     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1104.2
            031000     MOVE   SPACES TO INF-ANSI-REFERENCE.                         IF1104.2
            031100     GO TO  FAIL-ROUTINE-EX.                                      IF1104.2
            031200 FAIL-ROUTINE-WRITE.                                              IF1104.2
            031300     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE.        IF1104.2
            031400     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE                  IF1104.2
            031500                              CORMA-ANSI-REFERENCE.               IF1104.2
            031600     IF CORRECT-MIN NOT EQUAL TO SPACES THEN                      IF1104.2
            031700           MOVE TEST-CORRECT-MIN TO PRINT-REC PERFORM WRITE-LINE  IF1104.2
            031800           MOVE TEST-CORRECT-MAX TO PRINT-REC PERFORM WRITE-LINE  IF1104.2
            031900     ELSE                                                         IF1104.2
            032000           MOVE TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE.     IF1104.2
            032100     PERFORM WRITE-LINE.                                          IF1104.2
            032200     MOVE   SPACES TO COR-ANSI-REFERENCE.                         IF1104.2
            032300 FAIL-ROUTINE-EX. EXIT.                                           IF1104.2
            032400 BAIL-OUT.                                                        IF1104.2
            032500     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   IF1104.2
            032600     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           IF1104.2
            032700 BAIL-OUT-WRITE.                                                  IF1104.2
            032800     MOVE CORRECT-A TO XXCORRECT.                                 IF1104.2
            032900     MOVE COMPUTED-A TO XXCOMPUTED.                               IF1104.2
            033000     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 IF1104.2
            033100     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   IF1104.2
            033200     MOVE   SPACES TO INF-ANSI-REFERENCE.                         IF1104.2
            033300 BAIL-OUT-EX. EXIT.                                               IF1104.2
            033400 CCVS1-EXIT.                                                      IF1104.2
            033500     EXIT.                                                        IF1104.2
            033600********************************************************          IF1104.2
            033700*                                                      *          IF1104.2
            033800*    Intrinsic Function Tests      IF110A - FACTORIAL  *          IF1104.2
            033900*                                                      *          IF1104.2
            034000********************************************************          IF1104.2
            034100 SECT-IF110A SECTION.                                             IF1104.2
            034200 F-FACTORIAL-INFO.                                                IF1104.2
            034300     MOVE     "See ref. A-43 2.14" TO ANSI-REFERENCE.             IF1104.2
            034400     MOVE     "FACTORIAL Function" TO FEATURE.                    IF1104.2
            034500*****************TEST (a) ******************************          IF1104.2
            034600 F-FACTORIAL-01.                                                  IF1104.2
            034700     MOVE ZERO TO WS-NUM.                                         IF1104.2
            034800 F-FACTORIAL-TEST-01.                                             IF1104.2
            034900     COMPUTE WS-NUM = FUNCTION FACTORIAL(0).                      IF1104.2
            035000     IF WS-NUM = 1 THEN                                           IF1104.2
            035100                    PERFORM PASS                                  IF1104.2
            035200     ELSE                                                         IF1104.2
            035300                    MOVE WS-NUM TO COMPUTED-N                     IF1104.2
            035400                    MOVE 1 TO CORRECT-N                           IF1104.2
            035500                    PERFORM FAIL.                                 IF1104.2
            035600     GO TO F-FACTORIAL-WRITE-01.                                  IF1104.2
            035700 F-FACTORIAL-DELETE-01.                                           IF1104.2
            035800     PERFORM  DE-LETE.                                            IF1104.2
            035900     GO TO    F-FACTORIAL-WRITE-01.                               IF1104.2
            036000 F-FACTORIAL-WRITE-01.                                            IF1104.2
            036100     MOVE "F-FACTORIAL-01" TO PAR-NAME.                           IF1104.2
            036200     PERFORM  PRINT-DETAIL.                                       IF1104.2
            036300*****************TEST (b) ******************************          IF1104.2
            036400 F-FACTORIAL-02.                                                  IF1104.2
            036500     EVALUATE FUNCTION FACTORIAL(3)                               IF1104.2
            036600     WHEN 6                                                       IF1104.2
            036700                    PERFORM PASS                                  IF1104.2
            036800     WHEN OTHER                                                   IF1104.2
            036900                    PERFORM FAIL.                                 IF1104.2
            037000     GO TO F-FACTORIAL-WRITE-02.                                  IF1104.2
            037100 F-FACTORIAL-DELETE-02.                                           IF1104.2
            037200     PERFORM  DE-LETE.                                            IF1104.2
            037300     GO TO    F-FACTORIAL-WRITE-02.                               IF1104.2
            037400 F-FACTORIAL-WRITE-02.                                            IF1104.2
            037500     MOVE "F-FACTORIAL-02" TO PAR-NAME.                           IF1104.2
            037600     PERFORM  PRINT-DETAIL.                                       IF1104.2
            037700*****************TEST (c) ******************************          IF1104.2
            037800 F-FACTORIAL-03.                                                  IF1104.2
            037900     IF FUNCTION FACTORIAL(A) = 120 THEN                          IF1104.2
            038000                    PERFORM PASS                                  IF1104.2
            038100     ELSE                                                         IF1104.2
            038200                    PERFORM FAIL.                                 IF1104.2
            038300     GO TO F-FACTORIAL-WRITE-03.                                  IF1104.2
            038400 F-FACTORIAL-DELETE-03.                                           IF1104.2
            038500     PERFORM  DE-LETE.                                            IF1104.2
            038600     GO TO    F-FACTORIAL-WRITE-03.                               IF1104.2
            038700 F-FACTORIAL-WRITE-03.                                            IF1104.2
            038800     MOVE "F-FACTORIAL-03" TO PAR-NAME.                           IF1104.2
            038900     PERFORM  PRINT-DETAIL.                                       IF1104.2
            039000*****************TEST (d) ******************************          IF1104.2
            039100 F-FACTORIAL-04.                                                  IF1104.2
            039200     MOVE ZERO TO WS-NUM.                                         IF1104.2
            039300 F-FACTORIAL-TEST-04.                                             IF1104.2
            039400     COMPUTE WS-NUM = FUNCTION FACTORIAL(IND(4)).                 IF1104.2
            039500     IF WS-NUM = 6 THEN                                           IF1104.2
            039600                    PERFORM PASS                                  IF1104.2
            039700     ELSE                                                         IF1104.2
            039800                    MOVE WS-NUM TO COMPUTED-N                     IF1104.2
            039900                    MOVE 6 TO CORRECT-N                           IF1104.2
            040000                    PERFORM FAIL.                                 IF1104.2
            040100     GO TO F-FACTORIAL-WRITE-04.                                  IF1104.2
            040200 F-FACTORIAL-DELETE-04.                                           IF1104.2
            040300     PERFORM  DE-LETE.                                            IF1104.2
            040400     GO TO    F-FACTORIAL-WRITE-04.                               IF1104.2
            040500 F-FACTORIAL-WRITE-04.                                            IF1104.2
            040600     MOVE "F-FACTORIAL-04" TO PAR-NAME.                           IF1104.2
            040700     PERFORM  PRINT-DETAIL.                                       IF1104.2
            040800*****************TEST (e) ******************************          IF1104.2
            040900 F-FACTORIAL-05.                                                  IF1104.2
            041000     MOVE ZERO TO WS-NUM.                                         IF1104.2
            041100 F-FACTORIAL-TEST-05.                                             IF1104.2
            041200     COMPUTE WS-NUM = FUNCTION FACTORIAL(IND(A)).                 IF1104.2
            041300     IF WS-NUM = 5040 THEN                                        IF1104.2
            041400                    PERFORM PASS                                  IF1104.2
            041500     ELSE                                                         IF1104.2
            041600                    MOVE WS-NUM TO COMPUTED-N                     IF1104.2
            041700                    MOVE 5040 TO CORRECT-N                        IF1104.2
            041800                    PERFORM FAIL.                                 IF1104.2
            041900     GO TO F-FACTORIAL-WRITE-05.                                  IF1104.2
            042000 F-FACTORIAL-DELETE-05.                                           IF1104.2
            042100     PERFORM  DE-LETE.                                            IF1104.2
            042200     GO TO    F-FACTORIAL-WRITE-05.                               IF1104.2
            042300 F-FACTORIAL-WRITE-05.                                            IF1104.2
            042400     MOVE "F-FACTORIAL-05" TO PAR-NAME.                           IF1104.2
            042500     PERFORM  PRINT-DETAIL.                                       IF1104.2
            042600*****************TEST (f) ******************************          IF1104.2
            042700 F-FACTORIAL-06.                                                  IF1104.2
            042800     MOVE ZERO TO WS-NUM.                                         IF1104.2
            042900 F-FACTORIAL-TEST-06.                                             IF1104.2
            043000     COMPUTE WS-NUM = FUNCTION FACTORIAL(                         IF1104.2
            043100                      FUNCTION FACTORIAL(3)).                     IF1104.2
            043200     IF WS-NUM = 720 THEN                                         IF1104.2
            043300                    PERFORM PASS                                  IF1104.2
            043400     ELSE                                                         IF1104.2
            043500                    MOVE WS-NUM TO COMPUTED-N                     IF1104.2
            043600                    MOVE 720 TO CORRECT-N                         IF1104.2
            043700                    PERFORM FAIL.                                 IF1104.2
            043800     GO TO F-FACTORIAL-WRITE-06.                                  IF1104.2
            043900 F-FACTORIAL-DELETE-06.                                           IF1104.2
            044000     PERFORM  DE-LETE.                                            IF1104.2
            044100     GO TO    F-FACTORIAL-WRITE-06.                               IF1104.2
            044200 F-FACTORIAL-WRITE-06.                                            IF1104.2
            044300     MOVE "F-FACTORIAL-06" TO PAR-NAME.                           IF1104.2
            044400     PERFORM  PRINT-DETAIL.                                       IF1104.2
            044500*****************TEST (g) ******************************          IF1104.2
            044600 F-FACTORIAL-07.                                                  IF1104.2
            044700     MOVE ZERO TO WS-NUM.                                         IF1104.2
            044800 F-FACTORIAL-TEST-07.                                             IF1104.2
            044900     COMPUTE WS-NUM = FUNCTION FACTORIAL(1) + B.                  IF1104.2
            045000     IF WS-NUM = 8 THEN                                           IF1104.2
            045100                    PERFORM PASS                                  IF1104.2
            045200     ELSE                                                         IF1104.2
            045300                    MOVE WS-NUM TO COMPUTED-N                     IF1104.2
            045400                    MOVE 8 TO CORRECT-N                           IF1104.2
            045500                    PERFORM FAIL.                                 IF1104.2
            045600     GO TO F-FACTORIAL-WRITE-07.                                  IF1104.2
            045700 F-FACTORIAL-DELETE-07.                                           IF1104.2
            045800     PERFORM  DE-LETE.                                            IF1104.2
            045900     GO TO    F-FACTORIAL-WRITE-07.                               IF1104.2
            046000 F-FACTORIAL-WRITE-07.                                            IF1104.2
            046100     MOVE "F-FACTORIAL-07" TO PAR-NAME.                           IF1104.2
            046200     PERFORM  PRINT-DETAIL.                                       IF1104.2
            046300*****************TEST (h) ******************************          IF1104.2
            046400 F-FACTORIAL-08.                                                  IF1104.2
            046500     MOVE ZERO TO WS-NUM.                                         IF1104.2
            046600 F-FACTORIAL-TEST-08.                                             IF1104.2
            046700     COMPUTE WS-NUM = FUNCTION FACTORIAL(4) +                     IF1104.2
            046800                      FUNCTION FACTORIAL(2).                      IF1104.2
            046900     IF WS-NUM = 26 THEN                                          IF1104.2
            047000                    PERFORM PASS                                  IF1104.2
            047100     ELSE                                                         IF1104.2
            047200                    MOVE WS-NUM TO COMPUTED-N                     IF1104.2
            047300                    MOVE 26 TO CORRECT-N                          IF1104.2
            047400                    PERFORM FAIL.                                 IF1104.2
            047500     GO TO F-FACTORIAL-WRITE-08.                                  IF1104.2
            047600 F-FACTORIAL-DELETE-08.                                           IF1104.2
            047700     PERFORM  DE-LETE.                                            IF1104.2
            047800     GO TO    F-FACTORIAL-WRITE-08.                               IF1104.2
            047900 F-FACTORIAL-WRITE-08.                                            IF1104.2
            048000     MOVE "F-FACTORIAL-08" TO PAR-NAME.                           IF1104.2
            048100     PERFORM  PRINT-DETAIL.                                       IF1104.2
            048200*****************SPECIAL PERFORM TEST**********************       IF1104.2
            048300 F-FACTORIAL-09.                                                  IF1104.2
            048400     MOVE ZERO TO WS-NUM.                                         IF1104.2
            048500     PERFORM F-FACTORIAL-TEST-09                                  IF1104.2
            048600       UNTIL FUNCTION FACTORIAL(ARG1) > 120.                      IF1104.2
            048700     PERFORM PASS.                                                IF1104.2
            048800     GO TO F-FACTORIAL-WRITE-09.                                  IF1104.2
            048900 F-FACTORIAL-TEST-09.                                             IF1104.2
            049000     COMPUTE ARG1 = ARG1 + 1.                                     IF1104.2
            049100 F-FACTORIAL-DELETE-09.                                           IF1104.2
            049200     PERFORM  DE-LETE.                                            IF1104.2
            049300     GO TO    F-FACTORIAL-WRITE-09.                               IF1104.2
            049400 F-FACTORIAL-WRITE-09.                                            IF1104.2
            049500     MOVE "F-FACTORIAL-09" TO PAR-NAME.                           IF1104.2
            049600     PERFORM  PRINT-DETAIL.                                       IF1104.2
            049700********************END OF TESTS***************                   IF1104.2
            049800 CCVS-EXIT SECTION.                                               IF1104.2
            049900 CCVS-999999.                                                     IF1104.2
            050000     GO TO CLOSE-FILES.                                           IF1104.2
                  *END-OF,IF110A                                                                  
        """)
    )
}