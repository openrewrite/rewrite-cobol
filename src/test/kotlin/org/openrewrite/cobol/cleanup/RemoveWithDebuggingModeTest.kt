package org.openrewrite.cobol.search

import org.junit.jupiter.api.Disabled
import org.junit.jupiter.api.Test
import org.openrewrite.cobol.Assertions.cobol
import org.openrewrite.cobol.Assertions.cobolCopy
import org.openrewrite.cobol.cleanup.RemoveWithDebuggingMode
import org.openrewrite.cobol.tree.CobolTest
import org.openrewrite.test.RecipeSpec

class RemoveWithDebuggingModeTest : CobolTest() {

    override fun defaults(spec: RecipeSpec) {
        spec.recipe(RemoveWithDebuggingMode())
    }

    @Test
    fun noChange() = rewriteRun(
        cobolCopy(getNistSource("CM101M.CBL"))
    )

    @Disabled("Add formatting for tokens that extend passed the content area.")
    @Test
    fun specialFormattingCase() = rewriteRun(
        cobol(
            """
                000001 IDENTIFICATION DIVISION.                                         
                000002 PROGRAM-ID.                                                      
                000003     CONTINUED.                                                   
                000004 ENVIRONMENT DIVISION.                                            
                000005 CONFIGURATION SECTION.                                           
                000006 SOURCE-COMPUTER.                                                 
                000007                                                          XXXXX082SHIFTED
                000008         WITH                                                     
                000009         DEBUGGING                                                
                000010         M                                                        
                000011-         O                                                       
                000012-          D                                                      
                000013-           E.                                                    
            """.trimIndent(),
            """
                000001 IDENTIFICATION DIVISION.                                         
                000002 PROGRAM-ID.                                                      
                000003     CONTINUED.                                                   
                000004 ENVIRONMENT DIVISION.                                            
                000005 CONFIGURATION SECTION.                                           
                000006 SOURCE-COMPUTER.                                                 
                000007                                                          XXXXX082SHIFTED
                000008 .                                                                
            """.trimIndent())
    )

    @Disabled("Requires removal of CommentArea added to EOF.")
    @Test
    fun isContinued() = rewriteRun(
        cobol(
            """
                000001 IDENTIFICATION DIVISION.                                         
                000002 PROGRAM-ID.                                                      
                000003     CONTINUED.                                                   
                000004 ENVIRONMENT DIVISION.                                            
                000005 CONFIGURATION SECTION.                                           
                000006 SOURCE-COMPUTER.                                                 
                000007     XXXXX082                                                     SHIFTED
                000008         WITH                                                     
                000009         DEBUGGING                                                
                000010         M                                                        
                000011-         O                                                       
                000012-          D                                                      
                000013-           E.                                                    
            """.trimIndent(),
            """
                000001 IDENTIFICATION DIVISION.                                         
                000002 PROGRAM-ID.                                                      
                000003     CONTINUED.                                                   
                000004 ENVIRONMENT DIVISION.                                            
                000005 CONFIGURATION SECTION.                                           
                000006 SOURCE-COMPUTER.                                                 
                000007     XXXXX082.                                                     SHIFTED
            """.trimIndent())
    )

    @Test
    fun db101a() = rewriteRun(
        cobolCopy(getNistSource("DB101A.CBL"),
            """
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
                005200     XXXXX082.                                                    DB1014.2
                005300 OBJECT-COMPUTER.                                                 DB1014.2
                005400     XXXXX083.                                                    DB1014.2
                005500 INPUT-OUTPUT SECTION.                                            DB1014.2
                005600 FILE-CONTROL.                                                    DB1014.2
                005700     SELECT PRINT-FILE ASSIGN TO                                  DB1014.2
                005800     XXXXX055.                                                    DB1014.2
                005900 DATA DIVISION.                                                   DB1014.2
                006000 FILE SECTION.                                                    DB1014.2
                006100 FD  PRINT-FILE                                                   DB1014.2
                006200     LABEL RECORDS                                                DB1014.2
                006300     XXXXX084                                                     DB1014.2
                006400     DATA RECORD IS PRINT-REC DUMMY-RECORD.                       DB1014.2
                006500 01  PRINT-REC PICTURE X(120).                                    DB1014.2
                006600 01  DUMMY-RECORD PICTURE X(120).                                 DB1014.2
                006700 WORKING-STORAGE SECTION.                                         DB1014.2
                006800 77  A PIC 9 COMP VALUE 1.                                        DB1014.2
                006900 77  B PIC 9 COMP VALUE 5.                                        DB1014.2
                007000 77  C PIC 9 COMP VALUE 9.                                        DB1014.2
                007100 77  D PIC 99 COMP.                                               DB1014.2
                007200 77  RESULT-FLAG PIC 99 VALUE 0.                                  DB1014.2
                007300 77  DBLINE-HOLD PIC X(6).                                        DB1014.2
                007400 77  DBNAME-HOLD PIC X(30).                                       DB1014.2
                007500 77  DBCONT-HOLD PIC X(30).                                       DB1014.2
                007600 77  FIVE PIC 9 COMP VALUE 5.                                     DB1014.2
                007700 01  SIZE-19.                                                     DB1014.2
                007800     02  FILLER PIC X.                                            DB1014.2
                007900     02  SIZE-18.                                                 DB1014.2
                008000         03  FILLER PIC X.                                        DB1014.2
                008100         03  SIZE-17.                                             DB1014.2
                008200             04  FILLER PIC X.                                    DB1014.2
                008300             04  SIZE-16.                                         DB1014.2
                008400                 05  FILLER PIC X.                                DB1014.2
                008500                 05  SIZE-15.                                     DB1014.2
                008600                     06  FILLER PIC X.                            DB1014.2
                008700                     06  SIZE-14.                                 DB1014.2
                008800                         07  FILLER PIC X.                        DB1014.2
                008900                         07  SIZE-13.                             DB1014.2
                009000                             08  FILLER PIC X.                    DB1014.2
                009100                             08  SIZE-12.                         DB1014.2
                009200                                 09  FILLER PIC XX.               DB1014.2
                009300                                 09  SIZE-10.                     DB1014.2
                009400                                     10  FILLER PICTURE X(5).     DB1014.2
                009500                                     10  SIZE-5 PICTURE X(5).     DB1014.2
                009600 01  TEST-RESULTS.                                                DB1014.2
                009700     02 FILLER                    PICTURE X VALUE SPACE.          DB1014.2
                009800     02 FEATURE                   PICTURE X(20) VALUE SPACE.      DB1014.2
                009900     02 FILLER                    PICTURE X VALUE SPACE.          DB1014.2
                010000     02 P-OR-F                    PICTURE X(5) VALUE SPACE.       DB1014.2
                010100     02 FILLER                    PICTURE X  VALUE SPACE.         DB1014.2
                010200     02  PAR-NAME.                                                DB1014.2
                010300       03 FILLER PICTURE X(12) VALUE SPACE.                       DB1014.2
                010400       03  PARDOT-X PICTURE X  VALUE SPACE.                       DB1014.2
                010500       03 DOTVALUE PICTURE 99  VALUE ZERO.                        DB1014.2
                010600       03 FILLER PIC X(5) VALUE SPACE.                            DB1014.2
                010700     02 FILLER PIC X(10) VALUE SPACE.                             DB1014.2
                010800     02 RE-MARK PIC X(61).                                        DB1014.2
                010900 01  TEST-COMPUTED.                                               DB1014.2
                011000     02 FILLER PIC X(30) VALUE SPACE.                             DB1014.2
                011100     02 FILLER PIC X(17) VALUE "       COMPUTED=".                DB1014.2
                011200     02 COMPUTED-X.                                               DB1014.2
                011300     03 COMPUTED-A                PICTURE X(20) VALUE SPACE.      DB1014.2
                011400     03 COMPUTED-N REDEFINES COMPUTED-A PICTURE -9(9).9(9).       DB1014.2
                011500     03 COMPUTED-0V18 REDEFINES COMPUTED-A  PICTURE -.9(18).      DB1014.2
                011600     03 COMPUTED-4V14 REDEFINES COMPUTED-A  PICTURE -9(4).9(14).  DB1014.2
                011700     03 COMPUTED-14V4 REDEFINES COMPUTED-A  PICTURE -9(14).9(4).  DB1014.2
                011800     03       CM-18V0 REDEFINES COMPUTED-A.                       DB1014.2
                011900         04 COMPUTED-18V0                   PICTURE -9(18).       DB1014.2
                012000         04 FILLER                          PICTURE X.            DB1014.2
                012100     03 FILLER PIC X(50) VALUE SPACE.                             DB1014.2
                012200 01  TEST-CORRECT.                                                DB1014.2
                012300     02 FILLER PIC X(30) VALUE SPACE.                             DB1014.2
                012400     02 FILLER PIC X(17) VALUE "       CORRECT =".                DB1014.2
                012500     02 CORRECT-X.                                                DB1014.2
                012600     03 CORRECT-A                 PICTURE X(20) VALUE SPACE.      DB1014.2
                012700     03 CORRECT-N REDEFINES CORRECT-A PICTURE -9(9).9(9).         DB1014.2
                012800     03 CORRECT-0V18 REDEFINES CORRECT-A    PICTURE -.9(18).      DB1014.2
                012900     03 CORRECT-4V14 REDEFINES CORRECT-A    PICTURE -9(4).9(14).  DB1014.2
                013000     03 CORRECT-14V4 REDEFINES CORRECT-A    PICTURE -9(14).9(4).  DB1014.2
                013100     03      CR-18V0 REDEFINES CORRECT-A.                         DB1014.2
                013200         04 CORRECT-18V0                    PICTURE -9(18).       DB1014.2
                013300         04 FILLER                          PICTURE X.            DB1014.2
                013400     03 FILLER PIC X(50) VALUE SPACE.                             DB1014.2
                013500 01  CCVS-C-1.                                                    DB1014.2
                013600     02 FILLER PICTURE IS X(99) VALUE IS " FEATURE              PADB1014.2
                013900-    "SS  PARAGRAPH-NAME                                          DB1014.2
                013700-    "        REMARKS".                                           DB1014.2
                013800     02 FILLER PICTURE IS X(20) VALUE IS SPACE.                   DB1014.2
                014000 01  CCVS-C-2.                                                    DB1014.2
                014100     02 FILLER PICTURE IS X VALUE IS SPACE.                       DB1014.2
                014200     02 FILLER PICTURE IS X(6) VALUE IS "TESTED".                 DB1014.2
                014300     02 FILLER PICTURE IS X(15) VALUE IS SPACE.                   DB1014.2
                014400     02 FILLER PICTURE IS X(4) VALUE IS "FAIL".                   DB1014.2
                014500     02 FILLER PICTURE IS X(94) VALUE IS SPACE.                   DB1014.2
                014600 01  REC-SKL-SUB PICTURE 9(2) VALUE ZERO.                         DB1014.2
                014700 01  REC-CT PICTURE 99 VALUE ZERO.                                DB1014.2
                014800 01  DELETE-CNT                   PICTURE 999  VALUE ZERO.        DB1014.2
                014900 01  ERROR-COUNTER PICTURE IS 999 VALUE IS ZERO.                  DB1014.2
                015000 01  INSPECT-COUNTER PIC 999 VALUE ZERO.                          DB1014.2
                015100 01  PASS-COUNTER PIC 999 VALUE ZERO.                             DB1014.2
                015200 01  TOTAL-ERROR PIC 999 VALUE ZERO.                              DB1014.2
                015300 01  ERROR-HOLD PIC 999 VALUE ZERO.                               DB1014.2
                015400 01  DUMMY-HOLD PIC X(120) VALUE SPACE.                           DB1014.2
                015500 01  RECORD-COUNT PIC 9(5) VALUE ZERO.                            DB1014.2
                015600 01  CCVS-H-1.                                                    DB1014.2
                015700     02  FILLER   PICTURE X(27)  VALUE SPACE.                     DB1014.2
                015800     02 FILLER PICTURE X(67) VALUE                                DB1014.2
                015900     " FEDERAL COMPILER TESTING CENTER COBOL COMPILER VALIDATION  DB1014.2
                016000-    " SYSTEM".                                                   DB1014.2
                016100     02  FILLER     PICTURE X(26)  VALUE SPACE.                   DB1014.2
                016200 01  CCVS-H-2.                                                    DB1014.2
                016300     02 FILLER PICTURE X(52) VALUE IS                             DB1014.2
                016400     "CCVS74 NCC  COPY, NOT FOR DISTRIBUTION.".                   DB1014.2
                016500     02 FILLER PICTURE IS X(19) VALUE IS "TEST RESULTS SET-  ".   DB1014.2
                016600     02 TEST-ID PICTURE IS X(9).                                  DB1014.2
                016700     02 FILLER PICTURE IS X(40) VALUE IS SPACE.                   DB1014.2
                016800 01  CCVS-H-3.                                                    DB1014.2
                016900     02  FILLER PICTURE X(34) VALUE                               DB1014.2
                017000     " FOR OFFICIAL USE ONLY    ".                                DB1014.2
                017100     02  FILLER PICTURE X(58) VALUE                               DB1014.2
                017200     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".DB1014.2
                017300     02  FILLER PICTURE X(28) VALUE                               DB1014.2
                017400     "  COPYRIGHT   1974 ".                                       DB1014.2
                017500 01  CCVS-E-1.                                                    DB1014.2
                017600     02 FILLER PICTURE IS X(52) VALUE IS SPACE.                   DB1014.2
                017700     02 FILLER PICTURE IS X(14) VALUE IS "END OF TEST-  ".        DB1014.2
                017800     02 ID-AGAIN PICTURE IS X(9).                                 DB1014.2
                017900     02 FILLER PICTURE X(45) VALUE IS                             DB1014.2
                018000     " NTIS DISTRIBUTION COBOL 74".                               DB1014.2
                018100 01  CCVS-E-2.                                                    DB1014.2
                018200     02  FILLER                   PICTURE X(31)  VALUE            DB1014.2
                018300     SPACE.                                                       DB1014.2
                018400     02  FILLER                   PICTURE X(21)  VALUE SPACE.     DB1014.2
                018500     02 CCVS-E-2-2.                                               DB1014.2
                018600         03 ERROR-TOTAL PICTURE IS XXX VALUE IS SPACE.            DB1014.2
                018700         03 FILLER PICTURE IS X VALUE IS SPACE.                   DB1014.2
                018800         03 ENDER-DESC PIC X(44) VALUE "ERRORS ENCOUNTERED".      DB1014.2
                018900 01  CCVS-E-3.                                                    DB1014.2
                019000     02  FILLER PICTURE X(22) VALUE                               DB1014.2
                019100     " FOR OFFICIAL USE ONLY".                                    DB1014.2
                019200     02  FILLER PICTURE X(12) VALUE SPACE.                        DB1014.2
                019300     02  FILLER PICTURE X(58) VALUE                               DB1014.2
                019400     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".DB1014.2
                019500     02  FILLER PICTURE X(13) VALUE SPACE.                        DB1014.2
                019600     02 FILLER PIC X(15) VALUE " COPYRIGHT 1974".                 DB1014.2
                019700 01  CCVS-E-4.                                                    DB1014.2
                019800     02 CCVS-E-4-1 PIC XXX VALUE SPACE.                           DB1014.2
                019900     02 FILLER PIC XXXX VALUE " OF ".                             DB1014.2
                020000     02 CCVS-E-4-2 PIC XXX VALUE SPACE.                           DB1014.2
                020100     02 FILLER PIC X(40) VALUE                                    DB1014.2
                020200      "  TESTS WERE EXECUTED SUCCESSFULLY".                       DB1014.2
                020300 01  XXINFO.                                                      DB1014.2
                020400     02 FILLER PIC X(30) VALUE "        *** INFORMATION  ***".    DB1014.2
                020500     02 INFO-TEXT.                                                DB1014.2
                020600     04 FILLER PIC X(20) VALUE SPACE.                             DB1014.2
                020700     04 XXCOMPUTED PIC X(20).                                     DB1014.2
                020800     04 FILLER PIC X(5) VALUE SPACE.                              DB1014.2
                020900     04 XXCORRECT PIC X(20).                                      DB1014.2
                021000 01  HYPHEN-LINE.                                                 DB1014.2
                021100     02 FILLER PICTURE IS X VALUE IS SPACE.                       DB1014.2
                021200     02 FILLER PICTURE IS X(65) VALUE IS "************************DB1014.2
                021300-    "*****************************************".                 DB1014.2
                021400     02 FILLER PICTURE IS X(54) VALUE IS "************************DB1014.2
                021500-    "******************************".                            DB1014.2
                021600 01  CCVS-PGM-ID PIC X(6) VALUE                                   DB1014.2
                021700     "DB101A".                                                    DB1014.2
                021800 PROCEDURE DIVISION.                                              DB1014.2
                021900 DECLARATIVES.                                                    DB1014.2
                022000 START-UP SECTION.                                                DB1014.2
                022100     USE FOR DEBUGGING ON CCVS1.                                  DB1014.2
                022200 BEGIN-START-UP.                                                  DB1014.2
                022300     MOVE 1 TO RESULT-FLAG.                                       DB1014.2
                022400 DB-COMMON.                                                       DB1014.2
                022500     MOVE DEBUG-LINE TO DBLINE-HOLD.                              DB1014.2
                022600     MOVE DEBUG-NAME TO DBNAME-HOLD.                              DB1014.2
                022700     MOVE DEBUG-CONTENTS TO DBCONT-HOLD.                          DB1014.2
                022800 FALL-THROUGH-AND-SERIES SECTION.                                 DB1014.2
                022900     USE FOR DEBUGGING ON FALL-THROUGH-TEST                       DB1014.2
                023000              PROC-SERIES-TEST.                                   DB1014.2
                023100 BEGIN-FALL-THROUGH-AND-SERIES.                                   DB1014.2
                023200     PERFORM DB-COMMON.                                           DB1014.2
                023300     MOVE 2 TO RESULT-FLAG.                                       DB1014.2
                023400 GO-TO SECTION.                                                   DB1014.2
                023500     USE FOR DEBUGGING ON GO-TO-TEST.                             DB1014.2
                023600 BEGIN-GO-TO.                                                     DB1014.2
                023700     PERFORM DB-COMMON.                                           DB1014.2
                023800     MOVE 3 TO RESULT-FLAG.                                       DB1014.2
                023900 ALTER-PARAGRAPH SECTION.                                         DB1014.2
                024000     USE FOR DEBUGGING ON ALTERABLE-PARAGRAPH.                    DB1014.2
                024100 BEGIN-ALTER-PARAGRAPH.                                           DB1014.2
                024200     PERFORM DB-COMMON.                                           DB1014.2
                024300     MOVE 4 TO RESULT-FLAG.                                       DB1014.2
                024400 LOOP-ITERATION SECTION.                                          DB1014.2
                024500     USE FOR DEBUGGING ON LOOP-ROUTINE.                           DB1014.2
                024600 BEGIN-LOOP-ITERATION.                                            DB1014.2
                024700     PERFORM DB-COMMON.                                           DB1014.2
                024800     ADD 1 TO RESULT-FLAG.                                        DB1014.2
                024900 PERFORM-THRU SECTION.                                            DB1014.2
                025000     USE FOR DEBUGGING ON DO-NOTHING-1.                           DB1014.2
                025100 BEGIN-PERFORM-THRU.                                              DB1014.2
                025200     PERFORM DB-COMMON.                                           DB1014.2
                025300     ADD 1 TO RESULT-FLAG.                                        DB1014.2
                025400 END DECLARATIVES.                                                DB1014.2
                025500******************************************************************DB1014.2
                025600*    THE DEBUG-LINE (INSPT) SUBTEST FOR THE TEST NAMED IN THE    *DB1014.2
                025700*    OUTPUT REPORT AS "START-PROGRAM-TEST" SHOULD POINT TO THE   *DB1014.2
                025800*    EXECUTABLE STATEMENT WHICH FOLLOWS THIS COMMENT SET AND     *DB1014.2
                025900*    WHICH READS, "OPEN OUTPUT PRINT-FILE."                      *DB1014.2
                026000******************************************************************DB1014.2
                026100 CCVS1 SECTION.                                                   DB1014.2
                026200 OPEN-FILES.                                                      DB1014.2
                026300     OPEN     OUTPUT PRINT-FILE.                                  DB1014.2
                026400     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   DB1014.2
                026500     MOVE    SPACE TO TEST-RESULTS.                               DB1014.2
                026600     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             DB1014.2
                026700     GO TO CCVS1-EXIT.                                            DB1014.2
                026800 CLOSE-FILES.                                                     DB1014.2
                026900     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   DB1014.2
                027000 TERMINATE-CCVS.                                                  DB1014.2
                027100S    EXIT PROGRAM.                                                DB1014.2
                027200STERMINATE-CALL.                                                  DB1014.2
                027300     STOP     RUN.                                                DB1014.2
                027400 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         DB1014.2
                027500 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           DB1014.2
                027600 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          DB1014.2
                027700 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-CNT.          DB1014.2
                027800     MOVE "****TEST DELETED****" TO RE-MARK.                      DB1014.2
                027900 PRINT-DETAIL.                                                    DB1014.2
                028000     IF REC-CT NOT EQUAL TO ZERO                                  DB1014.2
                028100             MOVE "." TO PARDOT-X                                 DB1014.2
                028200             MOVE REC-CT TO DOTVALUE.                             DB1014.2
                028300     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      DB1014.2
                028400     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               DB1014.2
                028500        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 DB1014.2
                028600          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 DB1014.2
                028700     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              DB1014.2
                028800     MOVE SPACE TO CORRECT-X.                                     DB1014.2
                028900     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         DB1014.2
                029000     MOVE     SPACE TO RE-MARK.                                   DB1014.2
                029100 HEAD-ROUTINE.                                                    DB1014.2
                029200     MOVE CCVS-H-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB1014.2
                029300     MOVE CCVS-H-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.   DB1014.2
                029400     MOVE CCVS-H-3 TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.   DB1014.2
                029500 COLUMN-NAMES-ROUTINE.                                            DB1014.2
                029600     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB1014.2
                029700     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB1014.2
                029800     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        DB1014.2
                029900 END-ROUTINE.                                                     DB1014.2
                030000     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.DB1014.2
                030100 END-RTN-EXIT.                                                    DB1014.2
                030200     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB1014.2
                030300 END-ROUTINE-1.                                                   DB1014.2
                030400      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      DB1014.2
                030500      ERROR-HOLD. ADD DELETE-CNT TO ERROR-HOLD.                   DB1014.2
                030600      ADD PASS-COUNTER TO ERROR-HOLD.                             DB1014.2
                030700*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   DB1014.2
                030800      MOVE PASS-COUNTER TO CCVS-E-4-1.                            DB1014.2
                030900      MOVE ERROR-HOLD TO CCVS-E-4-2.                              DB1014.2
                031000      MOVE CCVS-E-4 TO CCVS-E-2-2.                                DB1014.2
                031100      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           DB1014.2
                031200  END-ROUTINE-12.                                                 DB1014.2
                031300      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        DB1014.2
                031400     IF       ERROR-COUNTER IS EQUAL TO ZERO                      DB1014.2
                031500         MOVE "NO " TO ERROR-TOTAL                                DB1014.2
                031600         ELSE                                                     DB1014.2
                031700         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       DB1014.2
                031800     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           DB1014.2
                031900     PERFORM WRITE-LINE.                                          DB1014.2
                032000 END-ROUTINE-13.                                                  DB1014.2
                032100     IF DELETE-CNT IS EQUAL TO ZERO                               DB1014.2
                032200         MOVE "NO " TO ERROR-TOTAL  ELSE                          DB1014.2
                032300         MOVE DELETE-CNT TO ERROR-TOTAL.                          DB1014.2
                032400     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   DB1014.2
                032500     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB1014.2
                032600      IF   INSPECT-COUNTER EQUAL TO ZERO                          DB1014.2
                032700          MOVE "NO " TO ERROR-TOTAL                               DB1014.2
                032800      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   DB1014.2
                032900      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            DB1014.2
                033000      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          DB1014.2
                033100     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB1014.2
                033200 WRITE-LINE.                                                      DB1014.2
                033300     ADD 1 TO RECORD-COUNT.                                       DB1014.2
                033400Y    IF RECORD-COUNT GREATER 50                                   DB1014.2
                033500Y        MOVE DUMMY-RECORD TO DUMMY-HOLD                          DB1014.2
                033600Y        MOVE SPACE TO DUMMY-RECORD                               DB1014.2
                033700Y        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  DB1014.2
                033800Y        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             DB1014.2
                033900Y        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     DB1014.2
                034000Y        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          DB1014.2
                034100Y        MOVE DUMMY-HOLD TO DUMMY-RECORD                          DB1014.2
                034200Y        MOVE ZERO TO RECORD-COUNT.                               DB1014.2
                034300     PERFORM WRT-LN.                                              DB1014.2
                034400 WRT-LN.                                                          DB1014.2
                034500     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               DB1014.2
                034600     MOVE SPACE TO DUMMY-RECORD.                                  DB1014.2
                034700 BLANK-LINE-PRINT.                                                DB1014.2
                034800     PERFORM WRT-LN.                                              DB1014.2
                034900 FAIL-ROUTINE.                                                    DB1014.2
                035000     IF COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.   DB1014.2
                035100     IF CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.    DB1014.2
                035200     MOVE "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.    DB1014.2
                035300     MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.     DB1014.2
                035400     GO TO FAIL-ROUTINE-EX.                                       DB1014.2
                035500 FAIL-ROUTINE-WRITE.                                              DB1014.2
                035600     MOVE TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE           DB1014.2
                035700     MOVE TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES.   DB1014.2
                035800 FAIL-ROUTINE-EX. EXIT.                                           DB1014.2
                035900 BAIL-OUT.                                                        DB1014.2
                036000     IF COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.       DB1014.2
                036100     IF CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.               DB1014.2
                036200 BAIL-OUT-WRITE.                                                  DB1014.2
                036300     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  DB1014.2
                036400     MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.     DB1014.2
                036500 BAIL-OUT-EX. EXIT.                                               DB1014.2
                036600 CCVS1-EXIT.                                                      DB1014.2
                036700     EXIT.                                                        DB1014.2
                036800 START-PROGRAM-TEST.                                              DB1014.2
                036900     IF RESULT-FLAG IS NOT EQUAL TO 1                             DB1014.2
                037000         MOVE "USE PROCEDURE NOT EXECUTED" TO RE-MARK             DB1014.2
                037100         PERFORM FAIL                                             DB1014.2
                037200         GO TO START-PROGRAM-WRITE.                               DB1014.2
                037300     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1014.2
                037400     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1014.2
                037500     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1014.2
                037600     PERFORM INSPT.                                               DB1014.2
                037700     PERFORM START-PROGRAM-WRITE.                                 DB1014.2
                037800     MOVE DBNAME-HOLD TO SIZE-5.                                  DB1014.2
                037900     IF SIZE-5 IS EQUAL TO "CCVS1"                                DB1014.2
                038000         PERFORM PASS  ELSE                                       DB1014.2
                038100         MOVE "CCVS1" TO CORRECT-A                                DB1014.2
                038200         MOVE DBNAME-HOLD TO COMPUTED-A                           DB1014.2
                038300         PERFORM FAIL.                                            DB1014.2
                038400 START-PROGRAM-TEST-1.                                            DB1014.2
                038500     MOVE "DEBUG-NAME" TO RE-MARK.                                DB1014.2
                038600     PERFORM START-PROGRAM-WRITE.                                 DB1014.2
                038700     MOVE DBCONT-HOLD TO SIZE-13.                                 DB1014.2
                038800     IF SIZE-13 IS EQUAL TO "START PROGRAM"                       DB1014.2
                038900         PERFORM PASS ELSE                                        DB1014.2
                039000         MOVE "START PROGRAM" TO CORRECT-A                        DB1014.2
                039100         MOVE DBCONT-HOLD TO COMPUTED-A                           DB1014.2
                039200         PERFORM FAIL.                                            DB1014.2
                039300     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB1014.2
                039400     GO TO START-PROGRAM-WRITE.                                   DB1014.2
                039500 START-PROGRAM-DELETE.                                            DB1014.2
                039600     PERFORM DE-LETE.                                             DB1014.2
                039700 START-PROGRAM-WRITE.                                             DB1014.2
                039800     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1014.2
                039900     MOVE "START-PROGRAM-TEST" TO PAR-NAME.                       DB1014.2
                040000     PERFORM PRINT-DETAIL.                                        DB1014.2
                040100******************************************************************DB1014.2
                040200*    THE DEBUG-LINE (INSPT) SUBTEST FOR THE TEST NAMED IN THE    *DB1014.2
                040300*    OUTPUT REPORT AS "FALL-THROUGH-TEST" SHOULD POINT TO THE    *DB1014.2
                040400*    EXECUTABLE STATEMENT WHICH FOLLOWS THIS COMMENT SET AND     *DB1014.2
                040500*    WHICH READS, "MOVE 0 TO RESULT-FLAG."                       *DB1014.2
                040600******************************************************************DB1014.2
                040700     MOVE 0 TO RESULT-FLAG.                                       DB1014.2
                040800 FALL-THROUGH-TEST.                                               DB1014.2
                040900     IF RESULT-FLAG IS NOT EQUAL TO 2                             DB1014.2
                041000         MOVE "USE PROCEDURE NOT EXECUTED" TO RE-MARK             DB1014.2
                041100         PERFORM FAIL                                             DB1014.2
                041200         GO TO FALL-THROUGH-WRITE.                                DB1014.2
                041300     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1014.2
                041400     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1014.2
                041500     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1014.2
                041600     PERFORM INSPT.                                               DB1014.2
                041700     PERFORM FALL-THROUGH-WRITE.                                  DB1014.2
                041800     MOVE DBNAME-HOLD TO SIZE-17.                                 DB1014.2
                041900     IF SIZE-17 IS EQUAL TO "FALL-THROUGH-TEST"                   DB1014.2
                042000         PERFORM PASS ELSE                                        DB1014.2
                042100         MOVE "FALL-THROUGH-TEST" TO CORRECT-A                    DB1014.2
                042200         MOVE DBNAME-HOLD TO COMPUTED-A                           DB1014.2
                042300         PERFORM FAIL.                                            DB1014.2
                042400     MOVE "DEBUG-NAME" TO RE-MARK.                                DB1014.2
                042500     PERFORM FALL-THROUGH-WRITE.                                  DB1014.2
                042600     MOVE DBCONT-HOLD TO SIZE-12.                                 DB1014.2
                042700     IF SIZE-12 IS EQUAL TO "FALL THROUGH"                        DB1014.2
                042800         PERFORM PASS ELSE                                        DB1014.2
                042900         MOVE "FALL THROUGH" TO CORRECT-A                         DB1014.2
                043000         MOVE DBCONT-HOLD TO COMPUTED-A                           DB1014.2
                043100         PERFORM FAIL.                                            DB1014.2
                043200     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB1014.2
                043300     GO TO FALL-THROUGH-WRITE.                                    DB1014.2
                043400 FALL-THROUGH-DELETE.                                             DB1014.2
                043500     PERFORM DE-LETE.                                             DB1014.2
                043600 FALL-THROUGH-WRITE.                                              DB1014.2
                043700     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1014.2
                043800     MOVE "FALL-THROUGH-TEST" TO PAR-NAME.                        DB1014.2
                043900     PERFORM PRINT-DETAIL.                                        DB1014.2
                044000******************************************************************DB1014.2
                044100*    THE DEBUG-LINE (INSPT) SUBTEST FOR THE TEST NAMED IN THE    *DB1014.2
                044200*    OUTPUT REPORT AS "PROC-SERIES-TEST" SHOULD POINT TO THE     *DB1014.2
                044300*    EXECUTABLE STATEMENT WHICH FOLLOWS THIS COMMENT SET AND     *DB1014.2
                044400*    WHICH READS, "MOVE 0 TO RESULT-FLAG."                       *DB1014.2
                044500******************************************************************DB1014.2
                044600     MOVE 0 TO RESULT-FLAG.                                       DB1014.2
                044700 PROC-SERIES-TEST.                                                DB1014.2
                044800     IF RESULT-FLAG IS NOT EQUAL TO 2                             DB1014.2
                044900         MOVE "USE PROCEDURE NOT EXECUTED" TO RE-MARK             DB1014.2
                045000         PERFORM FAIL                                             DB1014.2
                045100         GO TO PROC-SERIES-WRITE.                                 DB1014.2
                045200     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1014.2
                045300     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1014.2
                045400     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1014.2
                045500     PERFORM INSPT.                                               DB1014.2
                045600     PERFORM PROC-SERIES-WRITE.                                   DB1014.2
                045700     MOVE DBNAME-HOLD TO SIZE-16.                                 DB1014.2
                045800     IF SIZE-16 IS EQUAL TO "PROC-SERIES-TEST"                    DB1014.2
                045900         PERFORM PASS ELSE                                        DB1014.2
                046000         MOVE "PROC-SERIES-TEST" TO CORRECT-A                     DB1014.2
                046100         MOVE DBNAME-HOLD TO COMPUTED-A                           DB1014.2
                046200         PERFORM FAIL.                                            DB1014.2
                046300     MOVE "DEBUG-NAME" TO RE-MARK.                                DB1014.2
                046400     PERFORM PROC-SERIES-WRITE.                                   DB1014.2
                046500     MOVE DBCONT-HOLD TO SIZE-12.                                 DB1014.2
                046600     IF SIZE-12 IS EQUAL TO "FALL THROUGH"                        DB1014.2
                046700         PERFORM PASS ELSE                                        DB1014.2
                046800         MOVE "FALL THROUGH" TO CORRECT-A                         DB1014.2
                046900         MOVE DBCONT-HOLD TO COMPUTED-A                           DB1014.2
                047000         PERFORM FAIL.                                            DB1014.2
                047100     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB1014.2
                047200     GO TO PROC-SERIES-WRITE.                                     DB1014.2
                047300 PROC-SERIES-DELETE.                                              DB1014.2
                047400     PERFORM DE-LETE.                                             DB1014.2
                047500 PROC-SERIES-WRITE.                                               DB1014.2
                047600     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1014.2
                047700     MOVE "PROC-SERIES-TEST" TO PAR-NAME.                         DB1014.2
                047800     PERFORM PRINT-DETAIL.                                        DB1014.2
                047900     MOVE 0 TO RESULT-FLAG.                                       DB1014.2
                048000******************************************************************DB1014.2
                048100*    THE DEBUG-LINE (INSPT) SUBTEST FOR THE TEST NAMED IN THE    *DB1014.2
                048200*    OUTPUT REPORT AS "GO-TO-TEST" SHOULD POINT TO THE           *DB1014.2
                048300*    EXECUTABLE STATEMENT WHICH FOLLOWS THIS COMMENT SET AND     *DB1014.2
                048400*    WHICH READS, "GO TO GO-TO-TEST.".                           *DB1014.2
                048500******************************************************************DB1014.2
                048600 ALTERABLE-PARAGRAPH.                                             DB1014.2
                048700     GO TO GO-TO-TEST.                                            DB1014.2
                048800 FILLER-PARAGRAPH.                                                DB1014.2
                048900     DISPLAY "ALTER FAILED AT ALTER-TEST-INIT".                   DB1014.2
                049000     PERFORM FAIL.                                                DB1014.2
                049100     GO TO ALTERED-GO-TO-TEST.                                    DB1014.2
                049200 GO-TO-TEST.                                                      DB1014.2
                049300     IF RESULT-FLAG IS NOT EQUAL TO 3                             DB1014.2
                049400         MOVE "USE PROCEDURE NOT EXECUTED" TO RE-MARK             DB1014.2
                049500         PERFORM FAIL                                             DB1014.2
                049600         GO TO GO-TO-WRITE.                                       DB1014.2
                049700     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1014.2
                049800     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1014.2
                049900     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1014.2
                050000     PERFORM INSPT.                                               DB1014.2
                050100     PERFORM GO-TO-WRITE.                                         DB1014.2
                050200     MOVE DBNAME-HOLD TO SIZE-10.                                 DB1014.2
                050300     IF SIZE-10 IS EQUAL TO "GO-TO-TEST"                          DB1014.2
                050400         PERFORM PASS ELSE                                        DB1014.2
                050500         MOVE DBNAME-HOLD TO COMPUTED-A                           DB1014.2
                050600         MOVE "GO-TO-TEST" TO CORRECT-A                           DB1014.2
                050700         PERFORM FAIL.                                            DB1014.2
                050800     MOVE "DEBUG-NAME" TO RE-MARK.                                DB1014.2
                050900     PERFORM GO-TO-WRITE.                                         DB1014.2
                051000     MOVE DBCONT-HOLD TO SIZE-12.                                 DB1014.2
                051100     IF       SIZE-12 IS EQUAL TO SPACE                           DB1014.2
                051200              PERFORM PASS                                        DB1014.2
                051300              ELSE                                                DB1014.2
                051400              PERFORM FAIL                                        DB1014.2
                051500              MOVE DBCONT-HOLD TO COMPUTED-A                      DB1014.2
                051600              MOVE "SPACES" TO CORRECT-A.                         DB1014.2
                051700     MOVE     "DEBUG-CONTENTS" TO RE-MARK.                        DB1014.2
                051800     GO TO     GO-TO-WRITE.                                       DB1014.2
                051900 GO-TO-DELETE.                                                    DB1014.2
                052000         PERFORM DE-LETE.                                         DB1014.2
                052100 GO-TO-WRITE.                                                     DB1014.2
                052200     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1014.2
                052300     MOVE "GO-TO-TEST" TO PAR-NAME.                               DB1014.2
                052400     PERFORM PRINT-DETAIL.                                        DB1014.2
                052500     MOVE 0 TO RESULT-FLAG.                                       DB1014.2
                052600******************************************************************DB1014.2
                052700*    THE DEBUG-LINE (INSPT) SUBTEST FOR THE TEST NAMED IN THE    *DB1014.2
                052800*    OUTPUT REPORT AS "ALTER-TEST" SHOULD POINT TO THE           *DB1014.2
                052900*    EXECUTABLE STATEMENT WHICH FOLLOWS THIS COMMENT SET AND     *DB1014.2
                053000*    WHICH READS, "ALTER ALTERABLE-PARAGRAPH TO PROCEED TO       *DB1014.2
                053100*    ALTERED-GO-TO-TEST.".                                       *DB1014.2
                053200******************************************************************DB1014.2
                053300 ALTER-TEST-INIT.                                                 DB1014.2
                053400     ALTER ALTERABLE-PARAGRAPH TO PROCEED TO ALTERED-GO-TO-TEST.  DB1014.2
                053500 ALTER-TEST.                                                      DB1014.2
                053600     IF RESULT-FLAG IS NOT EQUAL TO 4                             DB1014.2
                053700         MOVE "USE PROCEDURE NOT EXECUTED" TO RE-MARK             DB1014.2
                053800         PERFORM FAIL                                             DB1014.2
                053900         GO TO ALTER-WRITE.                                       DB1014.2
                054000     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1014.2
                054100     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1014.2
                054200     MOVE DBLINE-HOLD TO COMPUTED-A                               DB1014.2
                054300     PERFORM INSPT.                                               DB1014.2
                054400     PERFORM ALTER-WRITE.                                         DB1014.2
                054500     MOVE DBNAME-HOLD TO SIZE-19.                                 DB1014.2
                054600     IF SIZE-19 IS EQUAL TO "ALTERABLE-PARAGRAPH"                 DB1014.2
                054700         PERFORM PASS ELSE                                        DB1014.2
                054800         MOVE "ALTERABLE-PARAGRAPH" TO CORRECT-A                  DB1014.2
                054900         MOVE DBNAME-HOLD TO COMPUTED-A                           DB1014.2
                055000         PERFORM FAIL.                                            DB1014.2
                055100     MOVE "DEBUG-NAME" TO RE-MARK.                                DB1014.2
                055200     PERFORM ALTER-WRITE.                                         DB1014.2
                055300     MOVE DBCONT-HOLD TO SIZE-18.                                 DB1014.2
                055400     IF SIZE-18 IS EQUAL TO "ALTERED-GO-TO-TEST"                  DB1014.2
                055500         PERFORM PASS ELSE                                        DB1014.2
                055600         MOVE "ALTERED-GO-TO-TEST" TO CORRECT-A                   DB1014.2
                055700         MOVE DBCONT-HOLD TO COMPUTED-A                           DB1014.2
                055800         PERFORM FAIL.                                            DB1014.2
                055900     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB1014.2
                056000     GO TO ALTER-WRITE.                                           DB1014.2
                056100 ALTER-DELETE.                                                    DB1014.2
                056200     PERFORM DE-LETE.                                             DB1014.2
                056300 ALTER-WRITE.                                                     DB1014.2
                056400     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1014.2
                056500     MOVE "ALTER-TEST" TO PAR-NAME.                               DB1014.2
                056600     PERFORM PRINT-DETAIL.                                        DB1014.2
                056700     MOVE 0 TO RESULT-FLAG.                                       DB1014.2
                056800******************************************************************DB1014.2
                056900*    THE DEBUG-LINE (INSPT) SUBTEST FOR THE TEST NAMED IN THE    *DB1014.2
                057000*    OUTPUT REPORT AS "ALTERED-GO-TO-TEST" SHOULD POINT TO THE   *DB1014.2
                057100*    EXECUTABLE STATEMENT WHICH FOLLOWS THIS COMMENT SET AND     *DB1014.2
                057200*    WHICH READS, "GO TO ALTERABLE-PARAGRAPH.".                  *DB1014.2
                057300******************************************************************DB1014.2
                057400 ALTER-WRITE-END.                                                 DB1014.2
                057500     GO TO ALTERABLE-PARAGRAPH.                                   DB1014.2
                057600 ALTERED-GO-TO-TEST.                                              DB1014.2
                057700     IF RESULT-FLAG IS NOT EQUAL TO 4                             DB1014.2
                057800         MOVE "USE PROCEDURE NOT EXECUTED" TO RE-MARK             DB1014.2
                057900         PERFORM FAIL                                             DB1014.2
                058000         GO TO ALTERED-GO-TO-WRITE.                               DB1014.2
                058100     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1014.2
                058200     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1014.2
                058300     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1014.2
                058400     PERFORM INSPT.                                               DB1014.2
                058500     PERFORM ALTERED-GO-TO-WRITE.                                 DB1014.2
                058600     MOVE DBNAME-HOLD TO SIZE-19.                                 DB1014.2
                058700     IF SIZE-19 IS EQUAL TO "ALTERABLE-PARAGRAPH"                 DB1014.2
                058800         PERFORM PASS ELSE                                        DB1014.2
                058900         MOVE DBNAME-HOLD TO COMPUTED-A                           DB1014.2
                059000         MOVE "ALTERABLE-PARAGRAPH" TO CORRECT-A                  DB1014.2
                059100         PERFORM FAIL.                                            DB1014.2
                059200     MOVE "DEBUG-NAME" TO RE-MARK.                                DB1014.2
                059300     PERFORM ALTERED-GO-TO-WRITE.                                 DB1014.2
                059400     IF       DBCONT-HOLD EQUAL TO SPACE                          DB1014.2
                059500              PERFORM PASS                                        DB1014.2
                059600              ELSE                                                DB1014.2
                059700              PERFORM FAIL                                        DB1014.2
                059800              MOVE DBCONT-HOLD TO COMPUTED-A                      DB1014.2
                059900              MOVE "SPACES" TO CORRECT-A.                         DB1014.2
                060000     MOVE     "DEBUG-CONTENTS" TO RE-MARK.                        DB1014.2
                060100     GO TO ALTERED-GO-TO-WRITE.                                   DB1014.2
                060200 ALTERED-GO-TO-DELETE.                                            DB1014.2
                060300     PERFORM DE-LETE.                                             DB1014.2
                060400 ALTERED-GO-TO-WRITE.                                             DB1014.2
                060500     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1014.2
                060600     MOVE "ALTERED-GO-TO-TEST" TO PAR-NAME.                       DB1014.2
                060700     PERFORM PRINT-DETAIL.                                        DB1014.2
                060800     MOVE 0 TO RESULT-FLAG.                                       DB1014.2
                060900******************************************************************DB1014.2
                061000*    THE DEBUG-LINE (INSPT) SUBTEST FOR THE TEST NAMED IN THE    *DB1014.2
                061100*    OUTPUT REPORT AS "PERF-ITERATION-TEST" SHOULD POINT TO THE  *DB1014.2
                061200*    EXECUTABLE STATEMENT WHICH FOLLOWS THIS COMMENT SET AND     *DB1014.2
                061300*    WHICH READS, "PERFORM LOOP-ROUTINE FIVE TIMES.".            *DB1014.2
                061400******************************************************************DB1014.2
                061500 PERF-ITERATION-TEST.                                             DB1014.2
                061600     PERFORM LOOP-ROUTINE FIVE TIMES.                             DB1014.2
                061700     IF RESULT-FLAG IS NOT EQUAL TO 5                             DB1014.2
                061800         MOVE "05" TO CORRECT-A                                   DB1014.2
                061900         MOVE RESULT-FLAG TO COMPUTED-A                           DB1014.2
                062000         MOVE "NO. OF TIMES USE PROC EXECUTED" TO RE-MARK         DB1014.2
                062100         PERFORM FAIL                                             DB1014.2
                062200              ELSE                                                DB1014.2
                062300              MOVE "PROC EXECUTED FIVE TIMES" TO RE-MARK          DB1014.2
                062400              PERFORM PASS.                                       DB1014.2
                062500     IF RESULT-FLAG IS EQUAL TO 0                                 DB1014.2
                062600         GO TO PERF-ITERATION-WRITE                               DB1014.2
                062700         ELSE PERFORM PERF-ITERATION-WRITE.                       DB1014.2
                062800     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1014.2
                062900     PERFORM INSPT.                                               DB1014.2
                063000     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1014.2
                063100     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1014.2
                063200     PERFORM PERF-ITERATION-WRITE.                                DB1014.2
                063300     MOVE DBNAME-HOLD TO SIZE-12.                                 DB1014.2
                063400     IF SIZE-12 IS EQUAL TO "LOOP-ROUTINE"                        DB1014.2
                063500         PERFORM PASS ELSE                                        DB1014.2
                063600         MOVE "LOOP-ROUTINE" TO CORRECT-A                         DB1014.2
                063700         MOVE DBNAME-HOLD TO COMPUTED-A                           DB1014.2
                063800         PERFORM FAIL.                                            DB1014.2
                063900     MOVE "DEBUG-NAME" TO RE-MARK.                                DB1014.2
                064000     PERFORM PERF-ITERATION-WRITE.                                DB1014.2
                064100     MOVE DBCONT-HOLD TO SIZE-12.                                 DB1014.2
                064200     IF SIZE-12 IS EQUAL TO "PERFORM LOOP"                        DB1014.2
                064300         PERFORM PASS ELSE                                        DB1014.2
                064400         MOVE "PERFORM LOOP" TO CORRECT-A                         DB1014.2
                064500         MOVE DBCONT-HOLD TO COMPUTED-A                           DB1014.2
                064600         PERFORM FAIL.                                            DB1014.2
                064700     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB1014.2
                064800     GO TO PERF-ITERATION-WRITE.                                  DB1014.2
                064900 PERF-ITERATION-DELETE.                                           DB1014.2
                065000     PERFORM DE-LETE.                                             DB1014.2
                065100 PERF-ITERATION-WRITE.                                            DB1014.2
                065200     MOVE "PERF-ITERATION-TEST" TO PAR-NAME.                      DB1014.2
                065300     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1014.2
                065400     PERFORM PRINT-DETAIL.                                        DB1014.2
                065500     MOVE 0 TO RESULT-FLAG.                                       DB1014.2
                065600 PERF-ITERATION-END.                                              DB1014.2
                065700     GO TO PERFORM-THRU-TEST.                                     DB1014.2
                065800 LOOP-ROUTINE.                                                    DB1014.2
                065900**NESTED PERFORMS ARE USED HERE TO ATTEMPT TO PREVENT OPTIMIZER   DB1014.2
                066000* ACTION RESULTING IN LOOP UNFOLDING AND REDUCTION.               DB1014.2
                066100     PERFORM DO-NOTHING.                                          DB1014.2
                066200******************************************************************DB1014.2
                066300*    THE DEBUG-LINE (INSPT) SUBTEST FOR THE TEST NAMED IN THE    *DB1014.2
                066400*    OUTPUT REPORT AS "PERFORM-THRU-TEST" SHOULD POINT TO THE    *DB1014.2
                066500*    EXECUTABLE STATEMENT WHICH FOLLOWS THIS COMMENT SET AND     *DB1014.2
                066600*    WHICH READS, "ADD A B C GIVING D.".                         *DB1014.2
                066700******************************************************************DB1014.2
                066800 DO-NOTHING.                                                      DB1014.2
                066900     ADD A B C GIVING D.                                          DB1014.2
                067000 DO-NOTHING-1.                                                    DB1014.2
                067100     SUBTRACT A FROM B.                                           DB1014.2
                067200 PERFORM-THRU-TEST.                                               DB1014.2
                067300     PERFORM DO-NOTHING THRU DO-NOTHING-1 FIVE TIMES.             DB1014.2
                067400     IF RESULT-FLAG IS NOT EQUAL TO 5                             DB1014.2
                067500         MOVE "05" TO CORRECT-A                                   DB1014.2
                067600         MOVE RESULT-FLAG TO COMPUTED-A                           DB1014.2
                067700         MOVE "NO. OF TIMES USE PROC EXECUTED" TO RE-MARK         DB1014.2
                067800         PERFORM FAIL                                             DB1014.2
                067900              ELSE                                                DB1014.2
                068000              MOVE "PROC EXECUTED FIVE TIMES" TO RE-MARK          DB1014.2
                068100              PERFORM PASS.                                       DB1014.2
                068200     IF RESULT-FLAG IS EQUAL TO 0                                 DB1014.2
                068300         GO TO PERFORM-THRU-WRITE   ELSE                          DB1014.2
                068400         PERFORM PERFORM-THRU-WRITE.                              DB1014.2
                068500     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1014.2
                068600     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1014.2
                068700     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1014.2
                068800     PERFORM INSPT.                                               DB1014.2
                068900     PERFORM PERFORM-THRU-WRITE.                                  DB1014.2
                069000     MOVE DBNAME-HOLD TO SIZE-12.                                 DB1014.2
                069100     IF SIZE-12 IS EQUAL TO "DO-NOTHING-1"                        DB1014.2
                069200         PERFORM PASS   ELSE                                      DB1014.2
                069300         MOVE "DO-NOTHING-1" TO CORRECT-A                         DB1014.2
                069400         MOVE DBNAME-HOLD TO COMPUTED-A                           DB1014.2
                069500         PERFORM FAIL.                                            DB1014.2
                069600     MOVE "DEBUG-NAME" TO RE-MARK.                                DB1014.2
                069700     PERFORM PERFORM-THRU-WRITE.                                  DB1014.2
                069800     MOVE DBCONT-HOLD TO SIZE-12                                  DB1014.2
                069900     IF SIZE-12 IS EQUAL TO "FALL THROUGH"                        DB1014.2
                070000         PERFORM PASS   ELSE                                      DB1014.2
                070100         MOVE "FALL THROUGH" TO CORRECT-A                         DB1014.2
                070200         MOVE DBCONT-HOLD TO COMPUTED-A                           DB1014.2
                070300         PERFORM FAIL.                                            DB1014.2
                070400     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB1014.2
                070500     GO TO PERFORM-THRU-WRITE.                                    DB1014.2
                070600 PERFORM-THRU-DELETE.                                             DB1014.2
                070700     PERFORM DE-LETE.                                             DB1014.2
                070800 PERFORM-THRU-WRITE.                                              DB1014.2
                070900     MOVE "PERFORM-THRU-TEST" TO PAR-NAME.                        DB1014.2
                071000     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1014.2
                071100     PERFORM PRINT-DETAIL.                                        DB1014.2
                071200     MOVE 0 TO RESULT-FLAG.                                       DB1014.2
                071300******************************************************************DB1014.2
                071400*    THE DEBUG-LINE (INSPT) SUBTEST FOR THE TEST NAMED IN THE    *DB1014.2
                071500*    OUTPUT REPORT AS "SIMPLE-PERFORM-TEST" SHOULD POINT TO THE  *DB1014.2
                071600*    EXECUTABLE STATEMENT WHICH FOLLOWS THIS COMMENT SET AND     *DB1014.2
                071700*    WHICH READS, "PERFORM LOOP-ROUTINE.".                       *DB1014.2
                071800******************************************************************DB1014.2
                071900 SIMPLE-PERFORM-TEST.                                             DB1014.2
                072000     PERFORM LOOP-ROUTINE.                                        DB1014.2
                072100     IF RESULT-FLAG IS NOT EQUAL TO 1                             DB1014.2
                072200         MOVE "USE PROCEDURE NOT EXECUTED" TO RE-MARK             DB1014.2
                072300         PERFORM FAIL                                             DB1014.2
                072400         GO TO SIMPLE-PERFORM-WRITE.                              DB1014.2
                072500     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1014.2
                072600     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1014.2
                072700     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1014.2
                072800     PERFORM INSPT.                                               DB1014.2
                072900     PERFORM SIMPLE-PERFORM-WRITE.                                DB1014.2
                073000     MOVE DBNAME-HOLD TO SIZE-12.                                 DB1014.2
                073100     IF SIZE-12 IS EQUAL TO "LOOP-ROUTINE"                        DB1014.2
                073200         PERFORM PASS   ELSE                                      DB1014.2
                073300         MOVE "LOOP-ROUTINE" TO CORRECT-A                         DB1014.2
                073400         MOVE DBNAME-HOLD TO COMPUTED-A                           DB1014.2
                073500         PERFORM FAIL.                                            DB1014.2
                073600     MOVE "DEBUG-NAME" TO RE-MARK.                                DB1014.2
                073700     PERFORM SIMPLE-PERFORM-WRITE.                                DB1014.2
                073800     MOVE DBCONT-HOLD TO SIZE-12.                                 DB1014.2
                073900     IF SIZE-12 IS EQUAL TO "PERFORM LOOP"                        DB1014.2
                074000         PERFORM PASS   ELSE                                      DB1014.2
                074100         MOVE "PERFORM LOOP" TO CORRECT-A                         DB1014.2
                074200         MOVE DBCONT-HOLD TO COMPUTED-A                           DB1014.2
                074300         PERFORM FAIL.                                            DB1014.2
                074400     MOVE "DEBUG-CONTENTS" TO RE-MARK.                            DB1014.2
                074500     GO TO SIMPLE-PERFORM-WRITE.                                  DB1014.2
                074600 SIMPLE-PERFORM-DELETE.                                           DB1014.2
                074700     PERFORM DE-LETE.                                             DB1014.2
                074800 SIMPLE-PERFORM-WRITE.                                            DB1014.2
                074900     MOVE "SIMPLE-PERFORM-TEST" TO PAR-NAME.                      DB1014.2
                075000     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1014.2
                075100     PERFORM PRINT-DETAIL.                                        DB1014.2
                075200     MOVE 0 TO RESULT-FLAG.                                       DB1014.2
                075300 DEBUG-LINE-TESTS-INIT.                                           DB1014.2
                075400     MOVE "DEBUG LINE TESTS" TO FEATURE.                          DB1014.2
                075500 DEBUG-LINE-TEST-01.                                              DB1014.2
                075600     MOVE "COMPLETE ENTITY" TO RE-MARK.                           DB1014.2
                075700     PERFORM FAIL.                                                DB1014.2
                075800D    PERFORM PASS  SUBTRACT 1 FROM ERROR-COUNTER.                 DB1014.2
                075900     GO TO DEBUG-LINE-WRITE-01.                                   DB1014.2
                076000 DEBUG-LINE-DELETE-01.                                            DB1014.2
                076100     PERFORM DE-LETE.                                             DB1014.2
                076200 DEBUG-LINE-WRITE-01.                                             DB1014.2
                076300     MOVE "DEBUG-LINE-TEST-01" TO PAR-NAME.                       DB1014.2
                076400     PERFORM PRINT-DETAIL.                                        DB1014.2
                076500 DEBUG-LINE-TEST-02.                                              DB1014.2
                076600     MOVE "CONSECUTIVE DEBUG LINES" TO RE-MARK.                   DB1014.2
                076700     PERFORM FAIL.                                                DB1014.2
                076800D    PERFORM PASS.                                                DB1014.2
                076900D    SUBTRACT 1 FROM ERROR-COUNTER.                               DB1014.2
                077000     GO TO DEBUG-LINE-WRITE-02.                                   DB1014.2
                077100 DEBUG-LINE-DELETE-02.                                            DB1014.2
                077200     PERFORM DE-LETE.                                             DB1014.2
                077300 DEBUG-LINE-WRITE-02.                                             DB1014.2
                077400     MOVE "DEBUG-LINE-TEST-02" TO PAR-NAME.                       DB1014.2
                077500     PERFORM PRINT-DETAIL.                                        DB1014.2
                077600 DEBUG-LINE-TEST-03.                                              DB1014.2
                077700     MOVE "BROKEN STATEMENTS" TO RE-MARK.                         DB1014.2
                077800     PERFORM                                                      DB1014.2
                077900D        PASS.  GO TO DEBUG-LINE-WRITE-03.                        DB1014.2
                078000DDEBUG-LINE-TEST-03-A.    PERFORM                                 DB1014.2
                078100                             FAIL.                                DB1014.2
                078200                             GO TO DEBUG-LINE-WRITE-03.           DB1014.2
                078300 DEBUG-LINE-DELETE-03.                                            DB1014.2
                078400     PERFORM DE-LETE.                                             DB1014.2
                078500 DEBUG-LINE-WRITE-03.                                             DB1014.2
                078600     MOVE "DEBUG-LINE-TEST-03" TO PAR-NAME.                       DB1014.2
                078700     PERFORM PRINT-DETAIL.                                        DB1014.2
                078800 DEBUG-LINE-TEST-04.                                              DB1014.2
                078900     MOVE "NESTED COMMENTS" TO RE-MARK.                           DB1014.2
                079000D    PERFORM                                                      DB1014.2
                079100*        FAIL.  GO TO DEBUG-LINE-WRITE-04.                        DB1014.2
                079200*DEBUG-LINE-TEST-04-A.    PERFORM                                 DB1014.2
                079300D                         PASS.  GO TO DEBUG-LINE-WRITE-04.       DB1014.2
                079400 DEBUG-LINE-TEST-04-B.                                            DB1014.2
                079500     MOVE "    FAILURE 04B" TO COMPUTED-A.                        DB1014.2
                079600     PERFORM FAIL.                                                DB1014.2
                079700     GO TO DEBUG-LINE-WRITE-04.                                   DB1014.2
                079800 DEBUG-LINE-DELETE-04.                                            DB1014.2
                079900     PERFORM DE-LETE.                                             DB1014.2
                080000 DEBUG-LINE-WRITE-04.                                             DB1014.2
                080100     MOVE "DEBUG-LINE-TEST-04" TO PAR-NAME.                       DB1014.2
                080200     PERFORM PRINT-DETAIL.                                        DB1014.2
                080300 DEBUG-LINE-TEST-05.                                              DB1014.2
                080400     MOVE "NESTED INSIDE COMMENTS" TO RE-MARK.                    DB1014.2
                080500*    PERFORM FAIL.                                                DB1014.2
                080600*    GO TO DEBUG-LINE-WRITE-05.                                   DB1014.2
                080700*DEBUG-LINE-TEST-05-A.                                            DB1014.2
                080800D    PERFORM PASS.                                                DB1014.2
                080900D    GO TO DEBUG-LINE-WRITE-05.                                   DB1014.2
                081000*DEBUG-LINE-TEST-05-B.                                            DB1014.2
                081100*    MOVE "    FAILURE 05B" TO COMPUTED-A.                        DB1014.2
                081200*    PERFORM FAIL.  GO TO DEBUG-LINE-WRITE-05.                    DB1014.2
                081300 DEBUG-LINE-TEST-05-C.                                            DB1014.2
                081400     MOVE "    FAILURE 05C" TO COMPUTED-A.                        DB1014.2
                081500     PERFORM FAIL.   GO TO DEBUG-LINE-WRITE-05.                   DB1014.2
                081600 DEBUG-LINE-DELETE-05.                                            DB1014.2
                081700     PERFORM DE-LETE.                                             DB1014.2
                081800 DEBUG-LINE-WRITE-05.                                             DB1014.2
                081900     MOVE "DEBUG-LINE-TEST-05" TO PAR-NAME.                       DB1014.2
                082000     PERFORM PRINT-DETAIL.                                        DB1014.2
                082100 CCVS-EXIT SECTION.                                               DB1014.2
                082200 CCVS-999999.                                                     DB1014.2
                082300     GO TO CLOSE-FILES.                                           DB1014.2
            """.trimIndent())
    )

    @Test
    fun db102a() = rewriteRun(
        cobolCopy(getNistSource("DB102A.CBL"),
            """
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
                004600     XXXXX082.                                                    DB1024.2
                004700 OBJECT-COMPUTER.                                                 DB1024.2
                004800     XXXXX083.                                                    DB1024.2
                004900 INPUT-OUTPUT SECTION.                                            DB1024.2
                005000 FILE-CONTROL.                                                    DB1024.2
                005100     SELECT PRINT-FILE ASSIGN TO                                  DB1024.2
                005200     XXXXX055.                                                    DB1024.2
                005300 DATA DIVISION.                                                   DB1024.2
                005400 FILE SECTION.                                                    DB1024.2
                005500 FD  PRINT-FILE                                                   DB1024.2
                005600     LABEL RECORDS                                                DB1024.2
                005700     XXXXX084                                                     DB1024.2
                005800     DATA RECORD IS PRINT-REC DUMMY-RECORD.                       DB1024.2
                005900 01  PRINT-REC PICTURE X(120).                                    DB1024.2
                006000 01  DUMMY-RECORD PICTURE X(120).                                 DB1024.2
                006100 WORKING-STORAGE SECTION.                                         DB1024.2
                006200 77  A PIC 9 COMP VALUE 1.                                        DB1024.2
                006300 77  B PIC 9 COMP VALUE 5.                                        DB1024.2
                006400 77  C PIC 9 COMP VALUE 9.                                        DB1024.2
                006500 77  D PIC 99 COMP.                                               DB1024.2
                006600 77  RESULT-FLAG PIC 99 COMP VALUE 0.                             DB1024.2
                006700 77  DBLINE-HOLD PIC X(6).                                        DB1024.2
                006800 77  DBNAME-HOLD PIC X(30).                                       DB1024.2
                006900 77  DBCONT-HOLD PIC X(30).                                       DB1024.2
                007000 77  FIVE PIC 9 COMP VALUE 5.                                     DB1024.2
                007100 01  SIZE-19.                                                     DB1024.2
                007200     02  FILLER PIC X.                                            DB1024.2
                007300     02  SIZE-18.                                                 DB1024.2
                007400         03  FILLER PIC X.                                        DB1024.2
                007500         03  SIZE-17.                                             DB1024.2
                007600             04  FILLER PIC X.                                    DB1024.2
                007700             04  SIZE-16.                                         DB1024.2
                007800                 05  FILLER PIC X.                                DB1024.2
                007900                 05  SIZE-15.                                     DB1024.2
                008000                     06  FILLER PIC X.                            DB1024.2
                008100                     06  SIZE-14.                                 DB1024.2
                008200                         07  FILLER PIC X.                        DB1024.2
                008300                         07  SIZE-13.                             DB1024.2
                008400                             08  FILLER PIC X.                    DB1024.2
                008500                             08  SIZE-12.                         DB1024.2
                008600                                 09  FILLER PIC X.                DB1024.2
                008700                                 09  SIZE-11.                     DB1024.2
                008800                                     10  FILLER PIC X.            DB1024.2
                008900                                     10  SIZE-10 PIC X(10).       DB1024.2
                009000 01  TEST-RESULTS.                                                DB1024.2
                009100     02 FILLER                    PICTURE X VALUE SPACE.          DB1024.2
                009200     02 FEATURE                   PICTURE X(20) VALUE SPACE.      DB1024.2
                009300     02 FILLER                    PICTURE X VALUE SPACE.          DB1024.2
                009400     02 P-OR-F                    PICTURE X(5) VALUE SPACE.       DB1024.2
                009500     02 FILLER                    PICTURE X  VALUE SPACE.         DB1024.2
                009600     02  PAR-NAME.                                                DB1024.2
                009700       03 FILLER PICTURE X(12) VALUE SPACE.                       DB1024.2
                009800       03  PARDOT-X PICTURE X  VALUE SPACE.                       DB1024.2
                009900       03 DOTVALUE PICTURE 99  VALUE ZERO.                        DB1024.2
                010000       03 FILLER PIC X(5) VALUE SPACE.                            DB1024.2
                010100     02 FILLER PIC X(10) VALUE SPACE.                             DB1024.2
                010200     02 RE-MARK PIC X(61).                                        DB1024.2
                010300 01  TEST-COMPUTED.                                               DB1024.2
                010400     02 FILLER PIC X(30) VALUE SPACE.                             DB1024.2
                010500     02 FILLER PIC X(17) VALUE "       COMPUTED=".                DB1024.2
                010600     02 COMPUTED-X.                                               DB1024.2
                010700     03 COMPUTED-A                PICTURE X(20) VALUE SPACE.      DB1024.2
                010800     03 COMPUTED-N REDEFINES COMPUTED-A PICTURE -9(9).9(9).       DB1024.2
                010900     03 COMPUTED-0V18 REDEFINES COMPUTED-A  PICTURE -.9(18).      DB1024.2
                011000     03 COMPUTED-4V14 REDEFINES COMPUTED-A  PICTURE -9(4).9(14).  DB1024.2
                011100     03 COMPUTED-14V4 REDEFINES COMPUTED-A  PICTURE -9(14).9(4).  DB1024.2
                011200     03       CM-18V0 REDEFINES COMPUTED-A.                       DB1024.2
                011300         04 COMPUTED-18V0                   PICTURE -9(18).       DB1024.2
                011400         04 FILLER                          PICTURE X.            DB1024.2
                011500     03 FILLER PIC X(50) VALUE SPACE.                             DB1024.2
                011600 01  TEST-CORRECT.                                                DB1024.2
                011700     02 FILLER PIC X(30) VALUE SPACE.                             DB1024.2
                011800     02 FILLER PIC X(17) VALUE "       CORRECT =".                DB1024.2
                011900     02 CORRECT-X.                                                DB1024.2
                012000     03 CORRECT-A                 PICTURE X(20) VALUE SPACE.      DB1024.2
                012100     03 CORRECT-N REDEFINES CORRECT-A PICTURE -9(9).9(9).         DB1024.2
                012200     03 CORRECT-0V18 REDEFINES CORRECT-A    PICTURE -.9(18).      DB1024.2
                012300     03 CORRECT-4V14 REDEFINES CORRECT-A    PICTURE -9(4).9(14).  DB1024.2
                012400     03 CORRECT-14V4 REDEFINES CORRECT-A    PICTURE -9(14).9(4).  DB1024.2
                012500     03      CR-18V0 REDEFINES CORRECT-A.                         DB1024.2
                012600         04 CORRECT-18V0                    PICTURE -9(18).       DB1024.2
                012700         04 FILLER                          PICTURE X.            DB1024.2
                012800     03 FILLER PIC X(50) VALUE SPACE.                             DB1024.2
                012900 01  CCVS-C-1.                                                    DB1024.2
                013000     02 FILLER PICTURE IS X(99) VALUE IS " FEATURE              PADB1024.2
                013300-    "SS  PARAGRAPH-NAME                                          DB1024.2
                013100-    "        REMARKS".                                           DB1024.2
                013200     02 FILLER PICTURE IS X(20) VALUE IS SPACE.                   DB1024.2
                013400 01  CCVS-C-2.                                                    DB1024.2
                013500     02 FILLER PICTURE IS X VALUE IS SPACE.                       DB1024.2
                013600     02 FILLER PICTURE IS X(6) VALUE IS "TESTED".                 DB1024.2
                013700     02 FILLER PICTURE IS X(15) VALUE IS SPACE.                   DB1024.2
                013800     02 FILLER PICTURE IS X(4) VALUE IS "FAIL".                   DB1024.2
                013900     02 FILLER PICTURE IS X(94) VALUE IS SPACE.                   DB1024.2
                014000 01  REC-SKL-SUB PICTURE 9(2) VALUE ZERO.                         DB1024.2
                014100 01  REC-CT PICTURE 99 VALUE ZERO.                                DB1024.2
                014200 01  DELETE-CNT                   PICTURE 999  VALUE ZERO.        DB1024.2
                014300 01  ERROR-COUNTER PICTURE IS 999 VALUE IS ZERO.                  DB1024.2
                014400 01  INSPECT-COUNTER PIC 999 VALUE ZERO.                          DB1024.2
                014500 01  PASS-COUNTER PIC 999 VALUE ZERO.                             DB1024.2
                014600 01  TOTAL-ERROR PIC 999 VALUE ZERO.                              DB1024.2
                014700 01  ERROR-HOLD PIC 999 VALUE ZERO.                               DB1024.2
                014800 01  DUMMY-HOLD PIC X(120) VALUE SPACE.                           DB1024.2
                014900 01  RECORD-COUNT PIC 9(5) VALUE ZERO.                            DB1024.2
                015000 01  CCVS-H-1.                                                    DB1024.2
                015100     02  FILLER   PICTURE X(27)  VALUE SPACE.                     DB1024.2
                015200     02 FILLER PICTURE X(67) VALUE                                DB1024.2
                015300     " FEDERAL COMPILER TESTING CENTER COBOL COMPILER VALIDATION  DB1024.2
                015400-    " SYSTEM".                                                   DB1024.2
                015500     02  FILLER     PICTURE X(26)  VALUE SPACE.                   DB1024.2
                015600 01  CCVS-H-2.                                                    DB1024.2
                015700     02 FILLER PICTURE X(52) VALUE IS                             DB1024.2
                015800     "CCVS74 NCC  COPY, NOT FOR DISTRIBUTION.".                   DB1024.2
                015900     02 FILLER PICTURE IS X(19) VALUE IS "TEST RESULTS SET-  ".   DB1024.2
                016000     02 TEST-ID PICTURE IS X(9).                                  DB1024.2
                016100     02 FILLER PICTURE IS X(40) VALUE IS SPACE.                   DB1024.2
                016200 01  CCVS-H-3.                                                    DB1024.2
                016300     02  FILLER PICTURE X(34) VALUE                               DB1024.2
                016400     " FOR OFFICIAL USE ONLY    ".                                DB1024.2
                016500     02  FILLER PICTURE X(58) VALUE                               DB1024.2
                016600     "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".DB1024.2
                016700     02  FILLER PICTURE X(28) VALUE                               DB1024.2
                016800     "  COPYRIGHT   1974 ".                                       DB1024.2
                016900 01  CCVS-E-1.                                                    DB1024.2
                017000     02 FILLER PICTURE IS X(52) VALUE IS SPACE.                   DB1024.2
                017100     02 FILLER PICTURE IS X(14) VALUE IS "END OF TEST-  ".        DB1024.2
                017200     02 ID-AGAIN PICTURE IS X(9).                                 DB1024.2
                017300     02 FILLER PICTURE X(45) VALUE IS                             DB1024.2
                017400     " NTIS DISTRIBUTION COBOL 74".                               DB1024.2
                017500 01  CCVS-E-2.                                                    DB1024.2
                017600     02  FILLER                   PICTURE X(31)  VALUE            DB1024.2
                017700     SPACE.                                                       DB1024.2
                017800     02  FILLER                   PICTURE X(21)  VALUE SPACE.     DB1024.2
                017900     02 CCVS-E-2-2.                                               DB1024.2
                018000         03 ERROR-TOTAL PICTURE IS XXX VALUE IS SPACE.            DB1024.2
                018100         03 FILLER PICTURE IS X VALUE IS SPACE.                   DB1024.2
                018200         03 ENDER-DESC PIC X(44) VALUE "ERRORS ENCOUNTERED".      DB1024.2
                018300 01  CCVS-E-3.                                                    DB1024.2
                018400     02  FILLER PICTURE X(22) VALUE                               DB1024.2
                018500     " FOR OFFICIAL USE ONLY".                                    DB1024.2
                018600     02  FILLER PICTURE X(12) VALUE SPACE.                        DB1024.2
                018700     02  FILLER PICTURE X(58) VALUE                               DB1024.2
                018800     "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".DB1024.2
                018900     02  FILLER PICTURE X(13) VALUE SPACE.                        DB1024.2
                019000     02 FILLER PIC X(15) VALUE " COPYRIGHT 1974".                 DB1024.2
                019100 01  CCVS-E-4.                                                    DB1024.2
                019200     02 CCVS-E-4-1 PIC XXX VALUE SPACE.                           DB1024.2
                019300     02 FILLER PIC XXXX VALUE " OF ".                             DB1024.2
                019400     02 CCVS-E-4-2 PIC XXX VALUE SPACE.                           DB1024.2
                019500     02 FILLER PIC X(40) VALUE                                    DB1024.2
                019600      "  TESTS WERE EXECUTED SUCCESSFULLY".                       DB1024.2
                019700 01  XXINFO.                                                      DB1024.2
                019800     02 FILLER PIC X(30) VALUE "        *** INFORMATION  ***".    DB1024.2
                019900     02 INFO-TEXT.                                                DB1024.2
                020000     04 FILLER PIC X(20) VALUE SPACE.                             DB1024.2
                020100     04 XXCOMPUTED PIC X(20).                                     DB1024.2
                020200     04 FILLER PIC X(5) VALUE SPACE.                              DB1024.2
                020300     04 XXCORRECT PIC X(20).                                      DB1024.2
                020400 01  HYPHEN-LINE.                                                 DB1024.2
                020500     02 FILLER PICTURE IS X VALUE IS SPACE.                       DB1024.2
                020600     02 FILLER PICTURE IS X(65) VALUE IS "************************DB1024.2
                020700-    "*****************************************".                 DB1024.2
                020800     02 FILLER PICTURE IS X(54) VALUE IS "************************DB1024.2
                020900-    "******************************".                            DB1024.2
                021000 01  CCVS-PGM-ID PIC X(6) VALUE                                   DB1024.2
                021100     "DB102A".                                                    DB1024.2
                021200 PROCEDURE DIVISION.                                              DB1024.2
                021300 DECLARATIVES.                                                    DB1024.2
                021400 START-UP SECTION.                                                DB1024.2
                021500     USE FOR DEBUGGING ON OPEN-FILES.                             DB1024.2
                021600 BEGIN-START-UP.                                                  DB1024.2
                021700     MOVE 1 TO RESULT-FLAG.                                       DB1024.2
                021800 DB-COMMON.                                                       DB1024.2
                021900     MOVE DEBUG-LINE TO DBLINE-HOLD.                              DB1024.2
                022000     MOVE DEBUG-NAME TO DBNAME-HOLD.                              DB1024.2
                022100     MOVE DEBUG-CONTENTS TO DBCONT-HOLD.                          DB1024.2
                022200 FALL-THROUGH-AND-SERIES SECTION.                                 DB1024.2
                022300     USE FOR DEBUGGING ON FALL-THROUGH-TEST                       DB1024.2
                022400              PROC-SERIES-TEST.                                   DB1024.2
                022500 BEGIN-FALL-THROUGH-AND-SERIES.                                   DB1024.2
                022600     PERFORM DB-COMMON.                                           DB1024.2
                022700     MOVE 2 TO RESULT-FLAG.                                       DB1024.2
                022800 GO-TO SECTION.                                                   DB1024.2
                022900     USE FOR DEBUGGING ON GO-TO-TEST.                             DB1024.2
                023000 BEGIN-GO-TO.                                                     DB1024.2
                023100     PERFORM DB-COMMON.                                           DB1024.2
                023200     MOVE 3 TO RESULT-FLAG.                                       DB1024.2
                023300 ALTER-PARAGRAPH SECTION.                                         DB1024.2
                023400     USE FOR DEBUGGING ON ALTERABLE-PARAGRAPH.                    DB1024.2
                023500 BEGIN-ALTER-PARAGRAPH.                                           DB1024.2
                023600     PERFORM DB-COMMON.                                           DB1024.2
                023700     MOVE 4 TO RESULT-FLAG.                                       DB1024.2
                023800 LOOP-ITERATION SECTION.                                          DB1024.2
                023900     USE FOR DEBUGGING ON LOOP-ROUTINE.                           DB1024.2
                024000 BEGIN-LOOP-ITERATION.                                            DB1024.2
                024100     PERFORM DB-COMMON.                                           DB1024.2
                024200     ADD 1 TO RESULT-FLAG.                                        DB1024.2
                024300 PERFORM-THRU SECTION.                                            DB1024.2
                024400     USE FOR DEBUGGING ON DO-NOTHING-1.                           DB1024.2
                024500 BEGIN-PERFORM-THRU.                                              DB1024.2
                024600     PERFORM DB-COMMON.                                           DB1024.2
                024700     ADD 1 TO RESULT-FLAG.                                        DB1024.2
                024800 END DECLARATIVES.                                                DB1024.2
                024900 CCVS1 SECTION.                                                   DB1024.2
                025000 OPEN-FILES.                                                      DB1024.2
                025100     OPEN     OUTPUT PRINT-FILE.                                  DB1024.2
                025200     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   DB1024.2
                025300     MOVE    SPACE TO TEST-RESULTS.                               DB1024.2
                025400     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             DB1024.2
                025500     GO TO CCVS1-EXIT.                                            DB1024.2
                025600 CLOSE-FILES.                                                     DB1024.2
                025700     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   DB1024.2
                025800 TERMINATE-CCVS.                                                  DB1024.2
                025900S    EXIT PROGRAM.                                                DB1024.2
                026000STERMINATE-CALL.                                                  DB1024.2
                026100     STOP     RUN.                                                DB1024.2
                026200 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         DB1024.2
                026300 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           DB1024.2
                026400 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          DB1024.2
                026500 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-CNT.          DB1024.2
                026600     MOVE "****TEST DELETED****" TO RE-MARK.                      DB1024.2
                026700 PRINT-DETAIL.                                                    DB1024.2
                026800     IF REC-CT NOT EQUAL TO ZERO                                  DB1024.2
                026900             MOVE "." TO PARDOT-X                                 DB1024.2
                027000             MOVE REC-CT TO DOTVALUE.                             DB1024.2
                027100     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      DB1024.2
                027200     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               DB1024.2
                027300        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 DB1024.2
                027400          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 DB1024.2
                027500     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              DB1024.2
                027600     MOVE SPACE TO CORRECT-X.                                     DB1024.2
                027700     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         DB1024.2
                027800     MOVE     SPACE TO RE-MARK.                                   DB1024.2
                027900 HEAD-ROUTINE.                                                    DB1024.2
                028000     MOVE CCVS-H-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB1024.2
                028100     MOVE CCVS-H-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.   DB1024.2
                028200     MOVE CCVS-H-3 TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.   DB1024.2
                028300 COLUMN-NAMES-ROUTINE.                                            DB1024.2
                028400     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB1024.2
                028500     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB1024.2
                028600     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        DB1024.2
                028700 END-ROUTINE.                                                     DB1024.2
                028800     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.DB1024.2
                028900 END-RTN-EXIT.                                                    DB1024.2
                029000     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   DB1024.2
                029100 END-ROUTINE-1.                                                   DB1024.2
                029200      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      DB1024.2
                029300      ERROR-HOLD. ADD DELETE-CNT TO ERROR-HOLD.                   DB1024.2
                029400      ADD PASS-COUNTER TO ERROR-HOLD.                             DB1024.2
                029500*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   DB1024.2
                029600      MOVE PASS-COUNTER TO CCVS-E-4-1.                            DB1024.2
                029700      MOVE ERROR-HOLD TO CCVS-E-4-2.                              DB1024.2
                029800      MOVE CCVS-E-4 TO CCVS-E-2-2.                                DB1024.2
                029900      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           DB1024.2
                030000  END-ROUTINE-12.                                                 DB1024.2
                030100      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        DB1024.2
                030200     IF       ERROR-COUNTER IS EQUAL TO ZERO                      DB1024.2
                030300         MOVE "NO " TO ERROR-TOTAL                                DB1024.2
                030400         ELSE                                                     DB1024.2
                030500         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       DB1024.2
                030600     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           DB1024.2
                030700     PERFORM WRITE-LINE.                                          DB1024.2
                030800 END-ROUTINE-13.                                                  DB1024.2
                030900     IF DELETE-CNT IS EQUAL TO ZERO                               DB1024.2
                031000         MOVE "NO " TO ERROR-TOTAL  ELSE                          DB1024.2
                031100         MOVE DELETE-CNT TO ERROR-TOTAL.                          DB1024.2
                031200     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   DB1024.2
                031300     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB1024.2
                031400      IF   INSPECT-COUNTER EQUAL TO ZERO                          DB1024.2
                031500          MOVE "NO " TO ERROR-TOTAL                               DB1024.2
                031600      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   DB1024.2
                031700      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            DB1024.2
                031800      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          DB1024.2
                031900     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           DB1024.2
                032000 WRITE-LINE.                                                      DB1024.2
                032100     ADD 1 TO RECORD-COUNT.                                       DB1024.2
                032200Y    IF RECORD-COUNT GREATER 50                                   DB1024.2
                032300Y        MOVE DUMMY-RECORD TO DUMMY-HOLD                          DB1024.2
                032400Y        MOVE SPACE TO DUMMY-RECORD                               DB1024.2
                032500Y        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  DB1024.2
                032600Y        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             DB1024.2
                032700Y        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     DB1024.2
                032800Y        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          DB1024.2
                032900Y        MOVE DUMMY-HOLD TO DUMMY-RECORD                          DB1024.2
                033000Y        MOVE ZERO TO RECORD-COUNT.                               DB1024.2
                033100     PERFORM WRT-LN.                                              DB1024.2
                033200 WRT-LN.                                                          DB1024.2
                033300     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               DB1024.2
                033400     MOVE SPACE TO DUMMY-RECORD.                                  DB1024.2
                033500 BLANK-LINE-PRINT.                                                DB1024.2
                033600     PERFORM WRT-LN.                                              DB1024.2
                033700 FAIL-ROUTINE.                                                    DB1024.2
                033800     IF COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.   DB1024.2
                033900     IF CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.    DB1024.2
                034000     MOVE "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.    DB1024.2
                034100     MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.     DB1024.2
                034200     GO TO FAIL-ROUTINE-EX.                                       DB1024.2
                034300 FAIL-ROUTINE-WRITE.                                              DB1024.2
                034400     MOVE TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE           DB1024.2
                034500     MOVE TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES.   DB1024.2
                034600 FAIL-ROUTINE-EX. EXIT.                                           DB1024.2
                034700 BAIL-OUT.                                                        DB1024.2
                034800     IF COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.       DB1024.2
                034900     IF CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.               DB1024.2
                035000 BAIL-OUT-WRITE.                                                  DB1024.2
                035100     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  DB1024.2
                035200     MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.     DB1024.2
                035300 BAIL-OUT-EX. EXIT.                                               DB1024.2
                035400 CCVS1-EXIT.                                                      DB1024.2
                035500     EXIT.                                                        DB1024.2
                035600 START-PROGRAM-TEST.                                              DB1024.2
                035700     IF RESULT-FLAG IS NOT EQUAL TO 0                             DB1024.2
                035800         MOVE "USE PROCEDURE EXECUTED" TO RE-MARK                 DB1024.2
                035900         PERFORM FAIL                                             DB1024.2
                036000         PERFORM START-PROGRAM-WRITE                              DB1024.2
                036100         ELSE PERFORM PASS                                        DB1024.2
                036200         GO TO START-PROGRAM-WRITE.                               DB1024.2
                036300     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
                036400     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1024.2
                036500     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1024.2
                036600     PERFORM START-PROGRAM-WRITE.                                 DB1024.2
                036700     MOVE "DEBUG-NAME; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
                036800     MOVE "<===  DEBUG-NAME" TO CORRECT-A.                        DB1024.2
                036900     MOVE DBNAME-HOLD TO COMPUTED-A.                              DB1024.2
                037000     PERFORM START-PROGRAM-WRITE.                                 DB1024.2
                037100     MOVE "DEBUG-CONTENTS; SEE NEXT LINE" TO RE-MARK.             DB1024.2
                037200     MOVE "<===  DEBUG-CONTENTS" TO CORRECT-A.                    DB1024.2
                037300     MOVE DBCONT-HOLD TO COMPUTED-A.                              DB1024.2
                037400     GO TO   START-PROGRAM-WRITE.                                 DB1024.2
                037500 START-PROGRAM-DELETE.                                            DB1024.2
                037600     PERFORM DE-LETE.                                             DB1024.2
                037700 START-PROGRAM-WRITE.                                             DB1024.2
                037800     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1024.2
                037900     MOVE "START-PROGRAM-TEST" TO PAR-NAME.                       DB1024.2
                038000     PERFORM PRINT-DETAIL.                                        DB1024.2
                038100     MOVE 0 TO RESULT-FLAG.                                       DB1024.2
                038200 FALL-THROUGH-TEST.                                               DB1024.2
                038300     IF RESULT-FLAG IS NOT EQUAL TO 0                             DB1024.2
                038400         MOVE "USE PROCEDURE EXECUTED" TO RE-MARK                 DB1024.2
                038500         PERFORM FAIL                                             DB1024.2
                038600         PERFORM FALL-THROUGH-WRITE                               DB1024.2
                038700         ELSE PERFORM PASS                                        DB1024.2
                038800         GO TO FALL-THROUGH-WRITE.                                DB1024.2
                038900     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
                039000     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1024.2
                039100     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1024.2
                039200     PERFORM FALL-THROUGH-WRITE.                                  DB1024.2
                039300     MOVE "DEBUG-NAME; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
                039400     MOVE "<===  DEBUG-NAME" TO CORRECT-A.                        DB1024.2
                039500     MOVE DBNAME-HOLD TO COMPUTED-A.                              DB1024.2
                039600     PERFORM FALL-THROUGH-WRITE.                                  DB1024.2
                039700     MOVE "DEBUG-CONTENTS; SEE NEXT LINE" TO RE-MARK.             DB1024.2
                039800     MOVE "<===  DEBUG-CONTENTS" TO CORRECT-A.                    DB1024.2
                039900     MOVE DBCONT-HOLD TO COMPUTED-A.                              DB1024.2
                040000     GO TO   FALL-THROUGH-WRITE.                                  DB1024.2
                040100 FALL-THROUGH-DELETE.                                             DB1024.2
                040200     PERFORM DE-LETE.                                             DB1024.2
                040300 FALL-THROUGH-WRITE.                                              DB1024.2
                040400     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1024.2
                040500     MOVE "FALL-THROUGH-TEST" TO PAR-NAME.                        DB1024.2
                040600     PERFORM PRINT-DETAIL.                                        DB1024.2
                040700     MOVE 0 TO RESULT-FLAG.                                       DB1024.2
                040800 PROC-SERIES-TEST.                                                DB1024.2
                040900     IF RESULT-FLAG IS NOT EQUAL TO 0                             DB1024.2
                041000         MOVE "USE PROCEDURE EXECUTED" TO RE-MARK                 DB1024.2
                041100         PERFORM FAIL                                             DB1024.2
                041200         PERFORM PROC-SERIES-WRITE                                DB1024.2
                041300         ELSE PERFORM PASS                                        DB1024.2
                041400         GO TO PROC-SERIES-WRITE.                                 DB1024.2
                041500     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
                041600     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1024.2
                041700     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1024.2
                041800     PERFORM PROC-SERIES-WRITE.                                   DB1024.2
                041900     MOVE "DEBUG-NAME; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
                042000     MOVE "<===  DEBUG-NAME" TO CORRECT-A.                        DB1024.2
                042100     MOVE DBNAME-HOLD TO COMPUTED-A.                              DB1024.2
                042200     PERFORM PROC-SERIES-WRITE.                                   DB1024.2
                042300     MOVE "DEBUG-CONTENTS; SEE NEXT LINE" TO RE-MARK.             DB1024.2
                042400     MOVE "<===  DEBUG-CONTENTS" TO CORRECT-A.                    DB1024.2
                042500     MOVE DBCONT-HOLD TO COMPUTED-A.                              DB1024.2
                042600     GO TO   PROC-SERIES-WRITE.                                   DB1024.2
                042700 PROC-SERIES-DELETE.                                              DB1024.2
                042800     PERFORM DE-LETE.                                             DB1024.2
                042900 PROC-SERIES-WRITE.                                               DB1024.2
                043000     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1024.2
                043100     MOVE "PROC-SERIES-TEST" TO PAR-NAME.                         DB1024.2
                043200     PERFORM PRINT-DETAIL.                                        DB1024.2
                043300     MOVE 0 TO RESULT-FLAG.                                       DB1024.2
                043400 ALTERABLE-PARAGRAPH.                                             DB1024.2
                043500     GO TO GO-TO-TEST.                                            DB1024.2
                043600 FILLER-PARAGRAPH.                                                DB1024.2
                043700     DISPLAY "ALTER FAILED AT ALTER-TEST-INIT".                   DB1024.2
                043800     PERFORM FAIL.                                                DB1024.2
                043900     GO TO ALTERED-GO-TO-TEST.                                    DB1024.2
                044000 GO-TO-TEST.                                                      DB1024.2
                044100     IF RESULT-FLAG IS NOT EQUAL TO 0                             DB1024.2
                044200         MOVE "USE PROCEDURE EXECUTED" TO RE-MARK                 DB1024.2
                044300         PERFORM FAIL                                             DB1024.2
                044400         PERFORM GO-TO-WRITE                                      DB1024.2
                044500         ELSE PERFORM PASS                                        DB1024.2
                044600         GO TO GO-TO-WRITE.                                       DB1024.2
                044700     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
                044800     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1024.2
                044900     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1024.2
                045000     PERFORM GO-TO-WRITE.                                         DB1024.2
                045100     MOVE "DEBUG-NAME; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
                045200     MOVE "<===  DEBUG-NAME" TO CORRECT-A.                        DB1024.2
                045300     MOVE DBNAME-HOLD TO COMPUTED-A.                              DB1024.2
                045400     PERFORM GO-TO-WRITE.                                         DB1024.2
                045500     MOVE "DEBUG-CONTENTS; SEE NEXT LINE" TO RE-MARK.             DB1024.2
                045600     MOVE "<===  DEBUG-CONTENTS" TO CORRECT-A.                    DB1024.2
                045700     MOVE DBCONT-HOLD TO COMPUTED-A.                              DB1024.2
                045800     GO TO   GO-TO-WRITE.                                         DB1024.2
                045900 GO-TO-DELETE.                                                    DB1024.2
                046000     PERFORM DE-LETE.                                             DB1024.2
                046100 GO-TO-WRITE.                                                     DB1024.2
                046200     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1024.2
                046300     MOVE "GO-TO-TEST" TO PAR-NAME.                               DB1024.2
                046400     PERFORM PRINT-DETAIL.                                        DB1024.2
                046500     MOVE 0 TO RESULT-FLAG.                                       DB1024.2
                046600 ALTER-TEST-INIT.                                                 DB1024.2
                046700     ALTER ALTERABLE-PARAGRAPH TO PROCEED TO ALTERED-GO-TO-TEST.  DB1024.2
                046800 ALTER-TEST.                                                      DB1024.2
                046900     IF RESULT-FLAG IS NOT EQUAL TO 0                             DB1024.2
                047000         MOVE "USE PROCEDURE EXECUTED" TO RE-MARK                 DB1024.2
                047100         PERFORM FAIL                                             DB1024.2
                047200         PERFORM ALTER-WRITE                                      DB1024.2
                047300         ELSE PERFORM PASS                                        DB1024.2
                047400         GO TO ALTER-WRITE.                                       DB1024.2
                047500     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
                047600     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1024.2
                047700     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1024.2
                047800     PERFORM ALTER-WRITE.                                         DB1024.2
                047900     MOVE "DEBUG-NAME; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
                048000     MOVE "<===  DEBUG-NAME" TO CORRECT-A.                        DB1024.2
                048100     MOVE DBNAME-HOLD TO COMPUTED-A.                              DB1024.2
                048200     PERFORM ALTER-WRITE.                                         DB1024.2
                048300     MOVE "DEBUG-CONTENTS; SEE NEXT LINE" TO RE-MARK.             DB1024.2
                048400     MOVE "<===  DEBUG-CONTENTS" TO CORRECT-A.                    DB1024.2
                048500     MOVE DBCONT-HOLD TO COMPUTED-A.                              DB1024.2
                048600     GO TO   ALTER-WRITE.                                         DB1024.2
                048700 ALTER-DELETE.                                                    DB1024.2
                048800     PERFORM DE-LETE.                                             DB1024.2
                048900 ALTER-WRITE.                                                     DB1024.2
                049000     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1024.2
                049100     MOVE "ALTER-TEST" TO PAR-NAME.                               DB1024.2
                049200     PERFORM PRINT-DETAIL.                                        DB1024.2
                049300     MOVE 0 TO RESULT-FLAG.                                       DB1024.2
                049400 ALTER-WRITE-END.                                                 DB1024.2
                049500     GO TO ALTERABLE-PARAGRAPH.                                   DB1024.2
                049600 ALTERED-GO-TO-TEST.                                              DB1024.2
                049700     IF RESULT-FLAG IS NOT EQUAL TO 0                             DB1024.2
                049800         MOVE "USE PROCEDURE EXECUTED" TO RE-MARK                 DB1024.2
                049900         PERFORM FAIL                                             DB1024.2
                050000         PERFORM ALTERED-GO-TO-WRITE                              DB1024.2
                050100         ELSE PERFORM PASS                                        DB1024.2
                050200         GO TO ALTERED-GO-TO-WRITE.                               DB1024.2
                050300     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
                050400     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1024.2
                050500     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1024.2
                050600     PERFORM ALTERED-GO-TO-WRITE.                                 DB1024.2
                050700     MOVE "DEBUG-NAME; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
                050800     MOVE "<===  DEBUG-NAME" TO CORRECT-A.                        DB1024.2
                050900     MOVE DBNAME-HOLD TO COMPUTED-A.                              DB1024.2
                051000     PERFORM ALTERED-GO-TO-WRITE.                                 DB1024.2
                051100     MOVE "DEBUG-CONTENTS; SEE NEXT LINE" TO RE-MARK.             DB1024.2
                051200     MOVE "<===  DEBUG-CONTENTS" TO CORRECT-A.                    DB1024.2
                051300     MOVE DBCONT-HOLD TO COMPUTED-A.                              DB1024.2
                051400     GO TO   ALTERED-GO-TO-WRITE.                                 DB1024.2
                051500 ALTERED-GO-TO-DELETE.                                            DB1024.2
                051600     PERFORM DE-LETE.                                             DB1024.2
                051700 ALTERED-GO-TO-WRITE.                                             DB1024.2
                051800     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1024.2
                051900     MOVE "ALTERED-GO-TO-TEST" TO PAR-NAME.                       DB1024.2
                052000     PERFORM PRINT-DETAIL.                                        DB1024.2
                052100     MOVE 0 TO RESULT-FLAG.                                       DB1024.2
                052200 PERF-ITERATION-TEST.                                             DB1024.2
                052300     PERFORM LOOP-ROUTINE FIVE TIMES.                             DB1024.2
                052400     IF RESULT-FLAG IS NOT EQUAL TO 0                             DB1024.2
                052500         MOVE "USE PROCEDURE EXECUTED" TO RE-MARK                 DB1024.2
                052600         PERFORM FAIL                                             DB1024.2
                052700         PERFORM PERF-ITERATION-WRITE                             DB1024.2
                052800         ELSE  PERFORM PASS                                       DB1024.2
                052900         GO TO PERF-ITERATION-WRITE.                              DB1024.2
                053000     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
                053100     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1024.2
                053200     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1024.2
                053300     PERFORM PERF-ITERATION-WRITE.                                DB1024.2
                053400     MOVE "DEBUG-NAME; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
                053500     MOVE "<===  DEBUG-NAME" TO CORRECT-A.                        DB1024.2
                053600     MOVE DBNAME-HOLD TO COMPUTED-A.                              DB1024.2
                053700     PERFORM PERF-ITERATION-WRITE.                                DB1024.2
                053800     MOVE "DEBUG-CONTENTS; SEE NEXT LINE" TO RE-MARK.             DB1024.2
                053900     MOVE "<===  DEBUG-CONTENTS" TO CORRECT-A.                    DB1024.2
                054000     MOVE DBCONT-HOLD TO COMPUTED-A.                              DB1024.2
                054100     GO TO   PERF-ITERATION-WRITE.                                DB1024.2
                054200 PERF-ITERATION-DELETE.                                           DB1024.2
                054300     PERFORM DE-LETE.                                             DB1024.2
                054400 PERF-ITERATION-WRITE.                                            DB1024.2
                054500     MOVE "PERF-ITERATION-TEST" TO PAR-NAME.                      DB1024.2
                054600     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1024.2
                054700     PERFORM PRINT-DETAIL.                                        DB1024.2
                054800     MOVE 0 TO RESULT-FLAG.                                       DB1024.2
                054900 PERF-ITERATION-END.                                              DB1024.2
                055000     GO TO PERFORM-THRU-TEST.                                     DB1024.2
                055100 LOOP-ROUTINE.                                                    DB1024.2
                055200**NESTED PERFORMS ARE USED HERE TO ATTEMPT TO PREVENT OPTIMIZER   DB1024.2
                055300* ACTION RESULTING IN LOOP UNFOLDING AND REDUCTION.               DB1024.2
                055400     PERFORM DO-NOTHING.                                          DB1024.2
                055500 DO-NOTHING.                                                      DB1024.2
                055600     ADD A B C GIVING D.                                          DB1024.2
                055700 DO-NOTHING-1.                                                    DB1024.2
                055800     SUBTRACT A FROM B.                                           DB1024.2
                055900 PERFORM-THRU-TEST.                                               DB1024.2
                056000     PERFORM DO-NOTHING THRU DO-NOTHING-1 FIVE TIMES.             DB1024.2
                056100     IF RESULT-FLAG IS NOT EQUAL TO 0                             DB1024.2
                056200         MOVE "USE PROCEDURE EXECUTED" TO RE-MARK                 DB1024.2
                056300         PERFORM FAIL                                             DB1024.2
                056400         PERFORM PERFORM-THRU-WRITE                               DB1024.2
                056500         ELSE PERFORM PASS                                        DB1024.2
                056600         GO TO PERFORM-THRU-WRITE.                                DB1024.2
                056700     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
                056800     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1024.2
                056900     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1024.2
                057000     PERFORM PERFORM-THRU-WRITE.                                  DB1024.2
                057100     MOVE "DEBUG-NAME; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
                057200     MOVE "<===  DEBUG-NAME" TO CORRECT-A.                        DB1024.2
                057300     MOVE DBNAME-HOLD TO COMPUTED-A.                              DB1024.2
                057400     PERFORM PERFORM-THRU-WRITE.                                  DB1024.2
                057500     MOVE "DEBUG-CONTENTS; SEE NEXT LINE" TO RE-MARK.             DB1024.2
                057600     MOVE "<===  DEBUG-CONTENTS" TO CORRECT-A.                    DB1024.2
                057700     MOVE DBCONT-HOLD TO COMPUTED-A.                              DB1024.2
                057800     GO TO   PERFORM-THRU-WRITE.                                  DB1024.2
                057900 PERFORM-THRU-DELETE.                                             DB1024.2
                058000     PERFORM DE-LETE.                                             DB1024.2
                058100 PERFORM-THRU-WRITE.                                              DB1024.2
                058200     MOVE "PERFORM-THRU-TEST" TO PAR-NAME.                        DB1024.2
                058300     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1024.2
                058400     PERFORM PRINT-DETAIL.                                        DB1024.2
                058500     MOVE 0 TO RESULT-FLAG.                                       DB1024.2
                058600 SIMPLE-PERFORM-TEST.                                             DB1024.2
                058700     PERFORM LOOP-ROUTINE.                                        DB1024.2
                058800     IF RESULT-FLAG IS NOT EQUAL TO 0                             DB1024.2
                058900         MOVE "USE PROCEDURE EXECUTED" TO RE-MARK                 DB1024.2
                059000         PERFORM FAIL                                             DB1024.2
                059100         PERFORM SIMPLE-PERFORM-WRITE                             DB1024.2
                059200         ELSE PERFORM PASS                                        DB1024.2
                059300         GO TO SIMPLE-PERFORM-WRITE.                              DB1024.2
                059400     MOVE "DEBUG-LINE; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
                059500     MOVE "<===  DEBUG-LINE" TO CORRECT-A.                        DB1024.2
                059600     MOVE DBLINE-HOLD TO COMPUTED-A.                              DB1024.2
                059700     PERFORM SIMPLE-PERFORM-WRITE.                                DB1024.2
                059800     MOVE "DEBUG-NAME; SEE NEXT LINE" TO RE-MARK.                 DB1024.2
                059900     MOVE "<===  DEBUG-NAME" TO CORRECT-A.                        DB1024.2
                060000     MOVE DBNAME-HOLD TO COMPUTED-A.                              DB1024.2
                060100     PERFORM SIMPLE-PERFORM-WRITE.                                DB1024.2
                060200     MOVE "DEBUG-CONTENTS; SEE NEXT LINE" TO RE-MARK.             DB1024.2
                060300     MOVE "<===  DEBUG-CONTENTS" TO CORRECT-A.                    DB1024.2
                060400     MOVE DBCONT-HOLD TO COMPUTED-A.                              DB1024.2
                060500     GO TO   SIMPLE-PERFORM-WRITE.                                DB1024.2
                060600 SIMPLE-PERFORM-DELETE.                                           DB1024.2
                060700     PERFORM DE-LETE.                                             DB1024.2
                060800 SIMPLE-PERFORM-WRITE.                                            DB1024.2
                060900     MOVE "SIMPLE-PERFORM-TEST" TO PAR-NAME.                      DB1024.2
                061000     MOVE "DEBUG ON PROC-NAME" TO FEATURE.                        DB1024.2
                061100     PERFORM PRINT-DETAIL.                                        DB1024.2
                061200     MOVE 0 TO RESULT-FLAG.                                       DB1024.2
                061300 DEBUG-LINE-TESTS-INIT.                                           DB1024.2
                061400     MOVE "DEBUG LINE TESTS" TO FEATURE.                          DB1024.2
                061500 DEBUG-LINE-TEST-01.                                              DB1024.2
                061600     MOVE "COMPLETE ENTITY" TO RE-MARK.                           DB1024.2
                061700     PERFORM FAIL.                                                DB1024.2
                061800D    PERFORM PASS  SUBTRACT 1 FROM ERROR-COUNTER.                 DB1024.2
                061900     GO TO DEBUG-LINE-WRITE-01.                                   DB1024.2
                062000 DEBUG-LINE-DELETE-01.                                            DB1024.2
                062100     PERFORM DE-LETE.                                             DB1024.2
                062200 DEBUG-LINE-WRITE-01.                                             DB1024.2
                062300     MOVE "DEBUG-LINE-TEST-01" TO PAR-NAME.                       DB1024.2
                062400     PERFORM PRINT-DETAIL.                                        DB1024.2
                062500 DEBUG-LINE-TEST-02.                                              DB1024.2
                062600     MOVE "CONSECUTIVE DEBUG LINES" TO RE-MARK.                   DB1024.2
                062700     PERFORM FAIL.                                                DB1024.2
                062800D    PERFORM PASS.                                                DB1024.2
                062900D    SUBTRACT 1 FROM ERROR-COUNTER.                               DB1024.2
                063000     GO TO DEBUG-LINE-WRITE-02.                                   DB1024.2
                063100 DEBUG-LINE-DELETE-02.                                            DB1024.2
                063200     PERFORM DE-LETE.                                             DB1024.2
                063300 DEBUG-LINE-WRITE-02.                                             DB1024.2
                063400     MOVE "DEBUG-LINE-TEST-02" TO PAR-NAME.                       DB1024.2
                063500     PERFORM PRINT-DETAIL.                                        DB1024.2
                063600 DEBUG-LINE-TEST-03.                                              DB1024.2
                063700     MOVE "BROKEN STATEMENTS" TO RE-MARK.                         DB1024.2
                063800     PERFORM                                                      DB1024.2
                063900D        PASS.  GO TO DEBUG-LINE-WRITE-03.                        DB1024.2
                064000DDEBUG-LINE-TEST-03-A.    PERFORM                                 DB1024.2
                064100                             FAIL.                                DB1024.2
                064200                             GO TO DEBUG-LINE-WRITE-03.           DB1024.2
                064300 DEBUG-LINE-DELETE-03.                                            DB1024.2
                064400     PERFORM DE-LETE.                                             DB1024.2
                064500 DEBUG-LINE-WRITE-03.                                             DB1024.2
                064600     MOVE "DEBUG-LINE-TEST-03" TO PAR-NAME.                       DB1024.2
                064700     PERFORM PRINT-DETAIL.                                        DB1024.2
                064800 DEBUG-LINE-TEST-04.                                              DB1024.2
                064900     MOVE "NESTED COMMENTS" TO RE-MARK.                           DB1024.2
                065000D    PERFORM                                                      DB1024.2
                065100*        FAIL.  GO TO DEBUG-LINE-WRITE-04.                        DB1024.2
                065200*DEBUG-LINE-TEST-04-A.    PERFORM                                 DB1024.2
                065300D                         PASS.  GO TO DEBUG-LINE-WRITE-04.       DB1024.2
                065400 DEBUG-LINE-TEST-04-B.                                            DB1024.2
                065500     MOVE "    FAILURE 04B" TO COMPUTED-A.                        DB1024.2
                065600     PERFORM FAIL.                                                DB1024.2
                065700     GO TO DEBUG-LINE-WRITE-04.                                   DB1024.2
                065800 DEBUG-LINE-DELETE-04.                                            DB1024.2
                065900     PERFORM DE-LETE.                                             DB1024.2
                066000 DEBUG-LINE-WRITE-04.                                             DB1024.2
                066100     MOVE "DEBUG-LINE-TEST-04" TO PAR-NAME.                       DB1024.2
                066200     PERFORM PRINT-DETAIL.                                        DB1024.2
                066300 DEBUG-LINE-TEST-05.                                              DB1024.2
                066400     MOVE "NESTED INSIDE COMMENTS" TO RE-MARK.                    DB1024.2
                066500*    PERFORM FAIL.                                                DB1024.2
                066600*    GO TO DEBUG-LINE-WRITE-05.                                   DB1024.2
                066700*DEBUG-LINE-TEST-05-A.                                            DB1024.2
                066800D    PERFORM PASS.                                                DB1024.2
                066900D    GO TO DEBUG-LINE-WRITE-05.                                   DB1024.2
                067000*DEBUG-LINE-TEST-05-B.                                            DB1024.2
                067100*    MOVE "    FAILURE 05B" TO COMPUTED-A.                        DB1024.2
                067200*    PERFORM FAIL.  GO TO DEBUG-LINE-WRITE-05.                    DB1024.2
                067300 DEBUG-LINE-TEST-05-C.                                            DB1024.2
                067400     MOVE "    FAILURE 05C" TO COMPUTED-A.                        DB1024.2
                067500     PERFORM FAIL.   GO TO DEBUG-LINE-WRITE-05.                   DB1024.2
                067600 DEBUG-LINE-DELETE-05.                                            DB1024.2
                067700     PERFORM DE-LETE.                                             DB1024.2
                067800 DEBUG-LINE-WRITE-05.                                             DB1024.2
                067900     MOVE "DEBUG-LINE-TEST-05" TO PAR-NAME.                       DB1024.2
                068000     PERFORM PRINT-DETAIL.                                        DB1024.2
                068100 CCVS-EXIT SECTION.                                               DB1024.2
                068200 CCVS-999999.                                                     DB1024.2
                068300     GO TO CLOSE-FILES.                                           DB1024.2
            """.trimIndent()
        )
    )
}