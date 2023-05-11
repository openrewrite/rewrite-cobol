/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.cobol.tree.cobol

import org.junit.jupiter.api.Test
import org.openrewrite.ExecutionContext
import org.openrewrite.cobol.internal.CobolPrinter
import org.openrewrite.cobol.tree.CobolTest
import org.openrewrite.cobol.tree.ParserAssertions.cobolCopy

class CobolParserReplaceTest : CobolTest() {

    companion object {
        val printer = CobolPrinter<ExecutionContext>(false, false)
    }

    @Test
    fun sm201A() =
        rewriteRun(
            cobolCopy(getNistSource("SM201A.CBL"), sm201A)
        )

    @Test
    fun sm202A() =
        rewriteRun(
            cobolCopy(getNistSource("SM202A.CBL"), sm202A)
        )

    @Test
    fun sm203A() =
        rewriteRun(
            cobolCopy(getNistSource("SM203A.CBL"), sm203A)
        )

    @Test
    fun sm205A() =
        rewriteRun(
            cobolCopy(getNistSource("SM205A.CBL"), sm205A)
        )

    @Test
    fun sm206A() =
        rewriteRun(
            cobolCopy(getNistSource("SM206A.CBL"), sm206A)
        )

    @Test
    fun sm208A() =
        rewriteRun(
            cobolCopy(getNistSource("SM208A.CBL"), sm208A)
        )

    @Test
    fun sm201ATrailingSub() =
        rewriteRun(
            cobolCopy(getNistSource("SM201A_TRAILING_SUB.CBL"), sm201A)
        )

    @Test
    fun additiveReplace() =
        rewriteRun(
            cobolCopy(getNistSource("ADDITIVE_REPLACE.CBL"),
                """
                IDENTIFICATION DIVISION.                                         
                PROGRAM-ID. SM208A.                                              
                ENVIRONMENT DIVISION.                                            
                DATA DIVISION.                                                   
                WORKING-STORAGE SECTION.                                         
                01    B     PICTURE                 S9(                                             
                7) COMP.
                01  C     PICTURE XXBXX/XX.                                      
                01  D     PICTURE X(7) VALUE "PICTURE".                          
                01  WRK-XN-00001  PIC X.                                         
                01  WRK-XN-00020  PIC X(20).                                     
                01  WRK-XN-00322  PIC X(322).                                    
                """.trimIndent()
            )
        )

    @Test
    fun reductiveReplace() =
        rewriteRun(
            cobolCopy(getNistSource("REDUCTIVE_REPLACE.CBL"),
                """
                IDENTIFICATION DIVISION.                                         
                PROGRAM-ID. SM208A.                                              
                ENVIRONMENT DIVISION.                                            
                DATA DIVISION.                                                   
                WORKING-STORAGE SECTION.                                         
                02  B     PICTURE S9(7) COMP.                                    
                03  C     PICTURE XXBXX/XX.                                      
                01  D     PICTURE X(7) VALUE "PICTURE".                          
                """.trimIndent()
            )
        )

    val sm201A =
        """
        IDENTIFICATION DIVISION.                                         
        PROGRAM-ID.                                                      
            SM201A.                                                      
        ENVIRONMENT DIVISION.                                            
        CONFIGURATION SECTION.                                           
        SOURCE-COMPUTER.                                                 
            XXXXX082.                                                    
        OBJECT-COMPUTER.                                                 
            XXXXX083.                                                    
        INPUT-OUTPUT SECTION.                                            
        FILE-CONTROL.                                                    
            SELECT PRINT-FILE ASSIGN TO                                  
            XXXXX055.                                                    
            SELECT TEST-FILE ASSIGN TO                                   
            XXXXP001.                                                    
        DATA DIVISION.                                                   
        FILE SECTION.                                                    
        FD  PRINT-FILE.                                                  
        01  PRINT-REC PICTURE X(120).                                    
        01  DUMMY-RECORD PICTURE X(120).                                 
        FD  TEST-FILE    LABEL RECORDS STANDARD                                       
            VALUE OF                                                     
            XXXXX074                                                     
            IS                                                           
            XXXXX075                                                     
            DATA RECORD IS TST-TEST.                                     
        01  TST-TEST            .                                                    
            02      TF-1 PICTURE 9(5).                                   
            02 FILLER    PICTURE X(115).                                 
        WORKING-STORAGE SECTION.                                         
        77  RCD-1 PICTURE 9(5) VALUE 97532.                              
        77  RCD-2 PICTURE 9(5) VALUE 23479.                              
        77  RCD-3 PICTURE 9(5) VALUE 10901.                              
        77  RCD-4 PICTURE 9(5) VALUE 02734.                              
        77  RCD-5 PICTURE 9(5) VALUE 14003.                              
        77  RCD-6 PICTURE 9(5) VALUE 19922.                              
        77  RCD-7 PICTURE 9(5) VALUE 03543.                              
        01  TEXT-TEST-1            .                                                    
            02          FILLER PICTURE                         X(                         115).                                   
            02    TXT-FLD-1                     PIC                         9(  5).                                 
        01  WSTR-1.                                                      
            02  WSTR-1A PICTURE XXX VALUE "ABC".                         
        01  WSTR-2.                                                      
            02 WSTR999 PICTURE X(3) VALUE                              "ABC".                                                        
        01  WSTR-3.                                                      
            02 WSTR-2A PICTURE X(3) VALUE                              "ABC".                                                        
        01  WSTR-4.    02 WSTR91  PICTURE XXX VALUE "ABC".                          
            02 WSTR92  PICTURE XXX VALUE "DEF".                          
            02 WSTR93  PICTURE XXX VALUE "GHI".                          
        01  WSTR-5.    02 WSTR4A  PICTURE XXX VALUE "ABC".                          
            02 WSTR4B  PICTURE XXX VALUE "DEF".                          
            02 WSTR4C  PICTURE XXX VALUE "GHI".                          
        01  TEST-RESULTS.                                                
            02 FILLER                   PIC X      VALUE SPACE.          
            02 FEATURE                  PIC X(20)  VALUE SPACE.          
            02 FILLER                   PIC X      VALUE SPACE.          
            02 P-OR-F                   PIC X(5)   VALUE SPACE.          
            02 FILLER                   PIC X      VALUE SPACE.          
            02  PAR-NAME.                                                
              03 FILLER                 PIC X(19)  VALUE SPACE.          
              03  PARDOT-X              PIC X      VALUE SPACE.          
              03 DOTVALUE               PIC 99     VALUE ZERO.           
            02 FILLER                   PIC X(8)   VALUE SPACE.          
            02 RE-MARK                  PIC X(61).                       
        01  TEST-COMPUTED.                                               
            02 FILLER                   PIC X(30)  VALUE SPACE.          
            02 FILLER                   PIC X(17)  VALUE                 
                   "       COMPUTED=".                                   
            02 COMPUTED-X.                                               
            03 COMPUTED-A               PIC X(20)  VALUE SPACE.          
            03 COMPUTED-N               REDEFINES COMPUTED-A             
                                        PIC -9(9).9(9).                  
            03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         
            03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     
            03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     
            03       CM-18V0 REDEFINES COMPUTED-A.                       
                04 COMPUTED-18V0                    PIC -9(18).          
                04 FILLER                           PIC X.               
            03 FILLER PIC X(50) VALUE SPACE.                             
        01  TEST-CORRECT.                                                
            02 FILLER PIC X(30) VALUE SPACE.                             
            02 FILLER PIC X(17) VALUE "       CORRECT =".                
            02 CORRECT-X.                                                
            03 CORRECT-A                  PIC X(20) VALUE SPACE.         
            03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      
            03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         
            03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     
            03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     
            03      CR-18V0 REDEFINES CORRECT-A.                         
                04 CORRECT-18V0                     PIC -9(18).          
                04 FILLER                           PIC X.               
            03 FILLER PIC X(2) VALUE SPACE.                              
            03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     
        01  CCVS-C-1.                                                    
            02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PASS  PARAGRAPH-NAME                                                 REMARKS".                                            
            02 FILLER                     PIC X(20)    VALUE SPACE.      
        01  CCVS-C-2.                                                    
            02 FILLER                     PIC X        VALUE SPACE.      
            02 FILLER                     PIC X(6)     VALUE "TESTED".   
            02 FILLER                     PIC X(15)    VALUE SPACE.      
            02 FILLER                     PIC X(4)     VALUE "FAIL".     
            02 FILLER                     PIC X(94)    VALUE SPACE.      
        01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       
        01  REC-CT                        PIC 99       VALUE ZERO.       
        01  DELETE-COUNTER                PIC 999      VALUE ZERO.       
        01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       
        01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       
        01  PASS-COUNTER                  PIC 999      VALUE ZERO.       
        01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       
        01  ERROR-HOLD                    PIC 999      VALUE ZERO.       
        01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      
        01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       
        01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     
        01  CCVS-H-1.                                                    
            02  FILLER                    PIC X(39)    VALUE SPACES.     
            02  FILLER                    PIC X(42)    VALUE             
            "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 
            02  FILLER                    PIC X(39)    VALUE SPACES.     
        01  CCVS-H-2A.                                                   
          02  FILLER                        PIC X(40)  VALUE SPACE.      
          02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  
          02  FILLER                        PIC XXXX   VALUE             
            "4.2 ".                                                      
          02  FILLER                        PIC X(28)  VALUE             
                   " COPY - NOT FOR DISTRIBUTION".                       
          02  FILLER                        PIC X(41)  VALUE SPACE.      
        01  CCVS-H-2B.                                                   
          02  FILLER                        PIC X(15)  VALUE             
                   "TEST RESULT OF ".                                    
          02  TEST-ID                       PIC X(9).                    
          02  FILLER                        PIC X(4)   VALUE             
                   " IN ".                                               
          02  FILLER                        PIC X(12)  VALUE             
            " HIGH       ".                                              
          02  FILLER                        PIC X(22)  VALUE             
                   " LEVEL VALIDATION FOR ".                             
          02  FILLER                        PIC X(58)  VALUE             
            "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".
        01  CCVS-H-3.                                                    
            02  FILLER                      PIC X(34)  VALUE             
                   " FOR OFFICIAL USE ONLY    ".                         
            02  FILLER                      PIC X(58)  VALUE             
            "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".
            02  FILLER                      PIC X(28)  VALUE             
                   "  COPYRIGHT   1985 ".                                
        01  CCVS-E-1.                                                    
            02 FILLER                       PIC X(52)  VALUE SPACE.      
            02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              
            02 ID-AGAIN                     PIC X(9).                    
            02 FILLER                       PIC X(45)  VALUE SPACES.     
        01  CCVS-E-2.                                                    
            02  FILLER                      PIC X(31)  VALUE SPACE.      
            02  FILLER                      PIC X(21)  VALUE SPACE.      
            02 CCVS-E-2-2.                                               
                03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      
                03 FILLER                   PIC X      VALUE SPACE.      
                03 ENDER-DESC               PIC X(44)  VALUE             
                   "ERRORS ENCOUNTERED".                                 
        01  CCVS-E-3.                                                    
            02  FILLER                      PIC X(22)  VALUE             
                   " FOR OFFICIAL USE ONLY".                             
            02  FILLER                      PIC X(12)  VALUE SPACE.      
            02  FILLER                      PIC X(58)  VALUE             
            "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".
            02  FILLER                      PIC X(13)  VALUE SPACE.      
            02 FILLER                       PIC X(15)  VALUE             
                    " COPYRIGHT 1985".                                   
        01  CCVS-E-4.                                                    
            02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      
            02 FILLER                       PIC X(4)   VALUE " OF ".     
            02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      
            02 FILLER                       PIC X(40)  VALUE             
             "  TESTS WERE EXECUTED SUCCESSFULLY".                       
        01  XXINFO.                                                      
            02 FILLER                       PIC X(19)  VALUE             
                   "*** INFORMATION ***".                                
            02 INFO-TEXT.                                                
              04 FILLER                     PIC X(8)   VALUE SPACE.      
              04 XXCOMPUTED                 PIC X(20).                   
              04 FILLER                     PIC X(5)   VALUE SPACE.      
              04 XXCORRECT                  PIC X(20).                   
            02 INF-ANSI-REFERENCE           PIC X(48).                   
        01  HYPHEN-LINE.                                                 
            02 FILLER  PIC IS X VALUE IS SPACE.                          
            02 FILLER  PIC IS X(65)    VALUE IS "*****************************************************************".                 
            02 FILLER  PIC IS X(54)    VALUE IS "******************************************************".                            
        01  CCVS-PGM-ID                     PIC X(9)   VALUE             
            "SM201A".                                                    
        PROCEDURE DIVISION.                                              
        CCVS1 SECTION.                                                   
        OPEN-FILES.                                                      
            OPEN     OUTPUT PRINT-FILE.                                  
            MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   
            MOVE    SPACE TO TEST-RESULTS.                               
            PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             
            GO TO CCVS1-EXIT.                                            
        CLOSE-FILES.                                                     
            PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   
        TERMINATE-CCVS.                                                  
            EXIT PROGRAM.                                                
        TERMINATE-CALL.                                                  
            STOP     RUN.                                                
        INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         
        PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           
        FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          
        DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      
            MOVE "****TEST DELETED****" TO RE-MARK.                      
        PRINT-DETAIL.                                                    
            IF REC-CT NOT EQUAL TO ZERO                                  
                    MOVE "." TO PARDOT-X                                 
                    MOVE REC-CT TO DOTVALUE.                             
            MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      
            IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               
               PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 
                 ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 
            MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              
            MOVE SPACE TO CORRECT-X.                                     
            IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         
            MOVE     SPACE TO RE-MARK.                                   
        HEAD-ROUTINE.                                                    
            MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  
            MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  
            MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  
            MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  
        COLUMN-NAMES-ROUTINE.                                            
            MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           
            MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   
            MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        
        END-ROUTINE.                                                     
            MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.
        END-RTN-EXIT.                                                    
            MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   
        END-ROUTINE-1.                                                   
             ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      
             ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               
             ADD PASS-COUNTER TO ERROR-HOLD.                             
             MOVE PASS-COUNTER TO CCVS-E-4-1.                            
             MOVE ERROR-HOLD TO CCVS-E-4-2.                              
             MOVE CCVS-E-4 TO CCVS-E-2-2.                                
             MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           
         END-ROUTINE-12.                                                 
             MOVE "TEST(S) FAILED" TO ENDER-DESC.                        
            IF       ERROR-COUNTER IS EQUAL TO ZERO                      
                MOVE "NO " TO ERROR-TOTAL                                
                ELSE                                                     
                MOVE ERROR-COUNTER TO ERROR-TOTAL.                       
            MOVE     CCVS-E-2 TO DUMMY-RECORD.                           
            PERFORM WRITE-LINE.                                          
        END-ROUTINE-13.                                                  
            IF DELETE-COUNTER IS EQUAL TO ZERO                           
                MOVE "NO " TO ERROR-TOTAL  ELSE                          
                MOVE DELETE-COUNTER TO ERROR-TOTAL.                      
            MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   
            MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           
             IF   INSPECT-COUNTER EQUAL TO ZERO                          
                 MOVE "NO " TO ERROR-TOTAL                               
             ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   
             MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            
             MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          
            MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           
        WRITE-LINE.                                                      
            ADD 1 TO RECORD-COUNT.                                       
            IF RECORD-COUNT GREATER 50                                   
                MOVE DUMMY-RECORD TO DUMMY-HOLD                          
                MOVE SPACE TO DUMMY-RECORD                               
                WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  
                MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             
                MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     
                MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          
                MOVE DUMMY-HOLD TO DUMMY-RECORD                          
                MOVE ZERO TO RECORD-COUNT.                               
            PERFORM WRT-LN.                                              
        WRT-LN.                                                          
            WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               
            MOVE SPACE TO DUMMY-RECORD.                                  
        BLANK-LINE-PRINT.                                                
            PERFORM WRT-LN.                                              
        FAIL-ROUTINE.                                                    
            IF   COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE. 
            IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.
            MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 
            MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   
            MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   
            MOVE   SPACES TO INF-ANSI-REFERENCE.                         
            GO TO  FAIL-ROUTINE-EX.                                      
        FAIL-ROUTINE-WRITE.                                              
            MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         
            MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 
            MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. 
            MOVE   SPACES TO COR-ANSI-REFERENCE.                         
        FAIL-ROUTINE-EX. EXIT.                                           
        BAIL-OUT.                                                        
            IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   
            IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           
        BAIL-OUT-WRITE.                                                  
            MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  
            MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 
            MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   
            MOVE   SPACES TO INF-ANSI-REFERENCE.                         
        BAIL-OUT-EX. EXIT.                                               
        CCVS1-EXIT.                                                      
            EXIT.                                                        
        INITIALIZATION SECTION.                                          
        SM201A-INIT.                                                     
            OPEN     OUTPUT TEST-FILE.                                   
            MOVE     "OUTPUT OF SM201A IS USED AS" TO RE-MARK.           
            PERFORM  PRINT-DETAIL.                                       
            MOVE     "INPUT FOR SM202A."           TO RE-MARK.           
            PERFORM  PRINT-DETAIL.                                       
            MOVE     "COPY 01 LEVEL --- " TO FEATURE.                    
            PERFORM  PRINT-DETAIL.                                       
        WORKING-STORAGE-TEST SECTION.                                    
        COPY-TEST-1.                                                     
            IF       WSTR-1A EQUAL TO WSTR999                            
                     PERFORM PASS GO TO COPY-WRITE-1.                    
            GO       TO COPY-FAIL-1.                                     
        COPY-DELETE-1.                                                   
            PERFORM  DE-LETE.                                            
            GO       TO COPY-WRITE-1.                                    
        COPY-FAIL-1.                                                     
            MOVE     WSTR999 TO COMPUTED-A.                              
            MOVE     "ABC" TO CORRECT-A.                                 
            PERFORM  FAIL.                                               
        COPY-WRITE-1.                                                    
            MOVE     "  REPLACING" TO FEATURE.                           
            MOVE     "COPY-TEST-1 " TO PAR-NAME.                         
            PERFORM  PRINT-DETAIL.                                       
        COPY-TEST-2.                                                     
            IF       WSTR-2A EQUAL TO "ABC"                              
                     PERFORM PASS GO TO COPY-WRITE-2.                    
            GO       TO COPY-FAIL-2.                                     
        COPY-DELETE-2.                                                   
            PERFORM  DE-LETE.                                            
            GO       TO COPY-WRITE-2.                                    
        COPY-FAIL-2.                                                     
            MOVE     WSTR-2A TO COMPUTED-A.                              
            MOVE     "ABC" TO CORRECT-A.                                 
            PERFORM  FAIL.                                               
        COPY-WRITE-2.                                                    
            MOVE     "  (NO REPLACING)" TO FEATURE.                      
            MOVE     "COPY-TEST-2 " TO PAR-NAME.                         
            PERFORM  PRINT-DETAIL.                                       
        COPY-INIT-A.                                                     
            MOVE     "  REPLACING" TO FEATURE.                           
        COPY-TEST-3.                                                     
            IF       WSTR91 EQUAL TO "ABC"                               
                     PERFORM PASS GO TO COPY-WRITE-3.                    
            GO       TO COPY-FAIL-3.                                     
        COPY-DELETE-3.                                                   
            PERFORM  DE-LETE.                                            
            GO       TO COPY-WRITE-3.                                    
        COPY-FAIL-3.                                                     
            MOVE     WSTR91 TO COMPUTED-A.                               
            MOVE     "ABC" TO CORRECT-A.                                 
            PERFORM  FAIL.                                               
        COPY-WRITE-3.                                                    
            MOVE     "COPY-TEST-3 " TO PAR-NAME.                         
            PERFORM  PRINT-DETAIL.                                       
        COPY-TEST-4.                                                     
            IF       WSTR92 EQUAL TO "DEF"                               
                     PERFORM PASS GO TO COPY-WRITE-4.                    
            GO       TO COPY-FAIL-4.                                     
        COPY-DELETE-4.                                                   
            PERFORM  DE-LETE.                                            
            GO       TO COPY-WRITE-4.                                    
        COPY-FAIL-4.                                                     
            MOVE     WSTR92 TO COMPUTED-A.                               
            MOVE     "DEF" TO CORRECT-A.                                 
            PERFORM  FAIL.                                               
        COPY-WRITE-4.                                                    
            MOVE     "COPY-TEST-4 " TO PAR-NAME.                         
            PERFORM  PRINT-DETAIL.                                       
        COPY-TEST-5.                                                     
            IF       WSTR93 EQUAL TO "GHI"                               
                     PERFORM PASS GO TO COPY-WRITE-5.                    
            GO       TO COPY-FAIL-5.                                     
        COPY-DELETE-5.                                                   
            PERFORM  DE-LETE.                                            
            GO       TO COPY-WRITE-5.                                    
        COPY-FAIL-5.                                                     
            MOVE     WSTR93 TO COMPUTED-A.                               
            MOVE     "GHI" TO CORRECT-A.                                 
            PERFORM  FAIL.                                               
        COPY-WRITE-5.                                                    
            MOVE     "COPY-TEST-5 " TO PAR-NAME.                         
            PERFORM  PRINT-DETAIL.                                       
        COPY-INIT-B.                                                     
            MOVE     "  (NOT REPLACING)" TO FEATURE.                     
        COPY-TEST-6.                                                     
            IF       WSTR4A EQUAL TO "ABC"                               
                     PERFORM PASS GO TO COPY-WRITE-6.                    
            GO       TO COPY-FAIL-6.                                     
        COPY-DELETE-6.                                                   
            PERFORM  DE-LETE.                                            
            GO       TO COPY-WRITE-6.                                    
        COPY-FAIL-6.                                                     
            MOVE     WSTR4A TO COMPUTED-A.                               
            MOVE     "ABC" TO CORRECT-A.                                 
            PERFORM  FAIL.                                               
        COPY-WRITE-6.                                                    
            MOVE     "COPY-TEST-6 " TO PAR-NAME.                         
            PERFORM  PRINT-DETAIL.                                       
        COPY-TEST-7.                                                     
            IF       WSTR4B EQUAL TO "DEF"                               
                     PERFORM PASS GO TO COPY-WRITE-7.                    
            GO       TO COPY-FAIL-7.                                     
        COPY-DELETE-7.                                                   
            PERFORM  DE-LETE.                                            
            GO       TO COPY-WRITE-7.                                    
        COPY-FAIL-7.                                                     
            MOVE     WSTR4B TO COMPUTED-A.                               
            MOVE     "DEF" TO CORRECT-A.                                 
            PERFORM  FAIL.                                               
        COPY-WRITE-7.                                                    
            MOVE     "COPY-TEST-7 " TO PAR-NAME.                         
            PERFORM  PRINT-DETAIL.                                       
        COPY-TEST-8.                                                     
            IF       WSTR4C EQUAL TO "GHI"                               
                     PERFORM PASS GO TO COPY-WRITE-8.                    
            GO       TO COPY-FAIL-8.                                     
        COPY-DELETE-8.                                                   
            PERFORM  DE-LETE.                                            
            GO       TO COPY-WRITE-8.                                    
        COPY-FAIL-8.                                                     
            MOVE     WSTR4C TO COMPUTED-A.                               
            MOVE     "GHI" TO CORRECT-A.                                 
            PERFORM  FAIL.                                               
        COPY-WRITE-8.                                                    
            MOVE     "COPY-TEST-8 " TO PAR-NAME.                         
            PERFORM  PRINT-DETAIL.                                       
        PARAGRAPH-TEST SECTION.                                          
        COPY-TEST-9.                                                     
            MOVE WSTR4B TO WSTR91.                                       
            MOVE WSTR4B TO WSTR93.                                       
            MOVE WSTR4A TO WSTR92.                                       
            IF       WSTR-4 EQUAL TO "DEFABCDEF"                         
                     PERFORM PASS GO TO COPY-WRITE-9.                    
            GO       TO COPY-FAIL-9.                                     
        COPY-DELETE-9.                                                   
            PERFORM  DE-LETE.                                            
            GO       TO COPY-WRITE-9.                                    
        COPY-FAIL-9.                                                     
            MOVE     WSTR-4 TO COMPUTED-A.                               
            MOVE     "DEFABCDEF" TO CORRECT-A.                           
            PERFORM  FAIL.                                               
        COPY-WRITE-9.                                                    
            MOVE     "COPY PARA REPLACING" TO FEATURE.                   
            MOVE     "COPY-TEST-9 " TO PAR-NAME.                         
            PERFORM  PRINT-DETAIL.                                       
        BUILD SECTION.                                                   
        COPY-TEST-10.                                                    
            MOVE     RCD-1 TO TF-1.                                      
            WRITE    TST-TEST.                                           
            MOVE     RCD-2 TO TF-1.                                      
            WRITE    TST-TEST.                                           
            MOVE     RCD-3 TO TF-1.                                      
            WRITE    TST-TEST.                                           
            MOVE     RCD-4 TO TF-1.                                      
            WRITE    TST-TEST.                                           
            MOVE     RCD-5 TO TF-1.                                      
            WRITE    TST-TEST.                                           
            MOVE     RCD-6 TO TF-1.                                      
            WRITE    TST-TEST.                                           
            MOVE     RCD-7 TO TF-1.                                      
            WRITE    TST-TEST.                                           
            PERFORM  PASS.                                               
            GO       TO COPY-WRITE-10.                                   
        COPY-DELETE-10.                                                  
            PERFORM  DE-LETE.                                            
        COPY-WRITE-10.                                                   
            MOVE     "COPY FD REPLACING" TO FEATURE.                     
            MOVE     "COPY-TEST-10 " TO PAR-NAME.                        
            MOVE     "OUTPUT PASSED ONTO SM202" TO RE-MARK.              
            PERFORM  PRINT-DETAIL.                                       
            CLOSE    TEST-FILE.                                          
        MORE-TESTS SECTION.                                              
        COPY-TEST-11.                                                    
            MOVE SPACES TO TEXT-TEST-1.                                  
            MOVE 12345 TO TXT-FLD-1.                                     
            IF TEXT-TEST-1 IS EQUAL TO "                                                                                                                   12345"        PERFORM PASS  ELSE  PERFORM FAIL.                        
            GO TO COPY-WRITE-11.                                         
        COPY-DELETE-11.                                                  
            PERFORM DE-LETE.                                             
        COPY-WRITE-11.                                                   
            MOVE "PSEUDO TEXT" TO FEATURE.                               
            MOVE "COPY-TEST-11" TO PAR-NAME.                             
            PERFORM PRINT-DETAIL.                                        
        CCVS-EXIT SECTION.                                               
        CCVS-999999.                                                     
            GO TO CLOSE-FILES.                                           
        """.trimIndent()

    val sm202A =
        """
        IDENTIFICATION DIVISION.                                         
        PROGRAM-ID.                                                      
            SM202A.                                                      
        ENVIRONMENT DIVISION.                                            
        CONFIGURATION SECTION.                                           
        SOURCE-COMPUTER.                                                 
            XXXXX082.                                                    
        OBJECT-COMPUTER.                                                 
            XXXXX083.                                                    
        INPUT-OUTPUT SECTION.                                            
        FILE-CONTROL.                                                    
            SELECT PRINT-FILE ASSIGN TO                                  
            XXXXX055.                                                    
            SELECT TEST-FILE ASSIGN TO                                   
            XXXXD001.                                                    
        DATA DIVISION.                                                   
        FILE SECTION.                                                    
        FD  PRINT-FILE.                                                  
        01  PRINT-REC PICTURE X(120).                                    
        01  DUMMY-RECORD PICTURE X(120).                                 
        FD  TEST-FILE                                                    
            LABEL RECORD STANDARD                                        
            VALUE OF                                                     
            XXXXX074                                                     
            IS                                                           
            XXXXX075                                                     
            DATA RECORD IS PROOF-REC.                                    
        01  PROOF-REC.                                                   
            02  TF-1 PICTURE 9(5).                                       
            02  FILLER PICTURE X(115).                                   
        WORKING-STORAGE SECTION.                                         
        01  COUNTER-16 PICTURE 9 VALUE 1.                                
        01  TOTAL-AREA.                                                  
            02 AREA-1          PICTURE AAAAA.                            
            02 AREA-2          PICTURE XXXXB.                            
            02 AREA-3          PICTURE XXXXX.                            
            02 AREA-4          PICTURE ZZZZZ.                            
        01  MISLEADING-DATA.                                             
            02 FALSE-DATA-1    PICTURE AAAAA VALUE "FALSE".              
            02 FALSE-DATA-2    PICTURE XXXXX VALUE " TENT".              
            02 FALSE-DATA-3    PICTURE XXXXX VALUE "- 5 =".              
            02 FALSE-DATA-4    PICTURE 99999 VALUE 00012.                
        01  QUALIFIED-DATA.                                              
            02 TRUE-Q-02.                                                
                03 TRUE-Q-03.                                            
                    04 TRUE-Q-04 PICTURE A(5) VALUE "TRUE ".             
                03 FALSE-Q-03.                                           
                    04 TRUE-Q-04 PICTURE A(5) VALUE "FIGHT".             
            02 FALSE-Q-02.                                               
                03 TRUE-Q-03.                                            
                    04 TRUE-Q-04 PICTURE A(5) VALUE "DRIVE".             
                03 FALSE-Q-03.                                           
                    04 TRUE-Q-04 PICTURE A(5) VALUE "THROW".             
        01  RE-SUB-DATA        PICTURE X(40) VALUE                       
            "ABCDEFGHIJKLMNOPQRST+ 2 =UVWXYZYXWVUTSRQ".                  
        01  SUBSCRIPTED-DATA REDEFINES RE-SUB-DATA.                      
            02 X OCCURS 2 TIMES.                                         
                03 Y OCCURS 2 TIMES.                                     
                    04 Z OCCURS 2 TIMES PICTURE X(5).                    
        01  TEST-RESULTS.                                                
            02 FILLER                   PIC X      VALUE SPACE.          
            02 FEATURE                  PIC X(20)  VALUE SPACE.          
            02 FILLER                   PIC X      VALUE SPACE.          
            02 P-OR-F                   PIC X(5)   VALUE SPACE.          
            02 FILLER                   PIC X      VALUE SPACE.          
            02  PAR-NAME.                                                
              03 FILLER                 PIC X(19)  VALUE SPACE.          
              03  PARDOT-X              PIC X      VALUE SPACE.          
              03 DOTVALUE               PIC 99     VALUE ZERO.           
            02 FILLER                   PIC X(8)   VALUE SPACE.          
            02 RE-MARK                  PIC X(61).                       
        01  TEST-COMPUTED.                                               
            02 FILLER                   PIC X(30)  VALUE SPACE.          
            02 FILLER                   PIC X(17)  VALUE                 
                   "       COMPUTED=".                                   
            02 COMPUTED-X.                                               
            03 COMPUTED-A               PIC X(20)  VALUE SPACE.          
            03 COMPUTED-N               REDEFINES COMPUTED-A             
                                        PIC -9(9).9(9).                  
            03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         
            03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     
            03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     
            03       CM-18V0 REDEFINES COMPUTED-A.                       
                04 COMPUTED-18V0                    PIC -9(18).          
                04 FILLER                           PIC X.               
            03 FILLER PIC X(50) VALUE SPACE.                             
        01  TEST-CORRECT.                                                
            02 FILLER PIC X(30) VALUE SPACE.                             
            02 FILLER PIC X(17) VALUE "       CORRECT =".                
            02 CORRECT-X.                                                
            03 CORRECT-A                  PIC X(20) VALUE SPACE.         
            03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      
            03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         
            03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     
            03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     
            03      CR-18V0 REDEFINES CORRECT-A.                         
                04 CORRECT-18V0                     PIC -9(18).          
                04 FILLER                           PIC X.               
            03 FILLER PIC X(2) VALUE SPACE.                              
            03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     
        01  CCVS-C-1.                                                    
            02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PASS  PARAGRAPH-NAME                                                 REMARKS".                                            
            02 FILLER                     PIC X(20)    VALUE SPACE.      
        01  CCVS-C-2.                                                    
            02 FILLER                     PIC X        VALUE SPACE.      
            02 FILLER                     PIC X(6)     VALUE "TESTED".   
            02 FILLER                     PIC X(15)    VALUE SPACE.      
            02 FILLER                     PIC X(4)     VALUE "FAIL".     
            02 FILLER                     PIC X(94)    VALUE SPACE.      
        01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       
        01  REC-CT                        PIC 99       VALUE ZERO.       
        01  DELETE-COUNTER                PIC 999      VALUE ZERO.       
        01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       
        01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       
        01  PASS-COUNTER                  PIC 999      VALUE ZERO.       
        01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       
        01  ERROR-HOLD                    PIC 999      VALUE ZERO.       
        01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      
        01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       
        01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     
        01  CCVS-H-1.                                                    
            02  FILLER                    PIC X(39)    VALUE SPACES.     
            02  FILLER                    PIC X(42)    VALUE             
            "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 
            02  FILLER                    PIC X(39)    VALUE SPACES.     
        01  CCVS-H-2A.                                                   
          02  FILLER                        PIC X(40)  VALUE SPACE.      
          02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  
          02  FILLER                        PIC XXXX   VALUE             
            "4.2 ".                                                      
          02  FILLER                        PIC X(28)  VALUE             
                   " COPY - NOT FOR DISTRIBUTION".                       
          02  FILLER                        PIC X(41)  VALUE SPACE.      
        01  CCVS-H-2B.                                                   
          02  FILLER                        PIC X(15)  VALUE             
                   "TEST RESULT OF ".                                    
          02  TEST-ID                       PIC X(9).                    
          02  FILLER                        PIC X(4)   VALUE             
                   " IN ".                                               
          02  FILLER                        PIC X(12)  VALUE             
            " HIGH       ".                                              
          02  FILLER                        PIC X(22)  VALUE             
                   " LEVEL VALIDATION FOR ".                             
          02  FILLER                        PIC X(58)  VALUE             
            "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".
        01  CCVS-H-3.                                                    
            02  FILLER                      PIC X(34)  VALUE             
                   " FOR OFFICIAL USE ONLY    ".                         
            02  FILLER                      PIC X(58)  VALUE             
            "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".
            02  FILLER                      PIC X(28)  VALUE             
                   "  COPYRIGHT   1985 ".                                
        01  CCVS-E-1.                                                    
            02 FILLER                       PIC X(52)  VALUE SPACE.      
            02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              
            02 ID-AGAIN                     PIC X(9).                    
            02 FILLER                       PIC X(45)  VALUE SPACES.     
        01  CCVS-E-2.                                                    
            02  FILLER                      PIC X(31)  VALUE SPACE.      
            02  FILLER                      PIC X(21)  VALUE SPACE.      
            02 CCVS-E-2-2.                                               
                03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      
                03 FILLER                   PIC X      VALUE SPACE.      
                03 ENDER-DESC               PIC X(44)  VALUE             
                   "ERRORS ENCOUNTERED".                                 
        01  CCVS-E-3.                                                    
            02  FILLER                      PIC X(22)  VALUE             
                   " FOR OFFICIAL USE ONLY".                             
            02  FILLER                      PIC X(12)  VALUE SPACE.      
            02  FILLER                      PIC X(58)  VALUE             
            "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".
            02  FILLER                      PIC X(13)  VALUE SPACE.      
            02 FILLER                       PIC X(15)  VALUE             
                    " COPYRIGHT 1985".                                   
        01  CCVS-E-4.                                                    
            02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      
            02 FILLER                       PIC X(4)   VALUE " OF ".     
            02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      
            02 FILLER                       PIC X(40)  VALUE             
             "  TESTS WERE EXECUTED SUCCESSFULLY".                       
        01  XXINFO.                                                      
            02 FILLER                       PIC X(19)  VALUE             
                   "*** INFORMATION ***".                                
            02 INFO-TEXT.                                                
              04 FILLER                     PIC X(8)   VALUE SPACE.      
              04 XXCOMPUTED                 PIC X(20).                   
              04 FILLER                     PIC X(5)   VALUE SPACE.      
              04 XXCORRECT                  PIC X(20).                   
            02 INF-ANSI-REFERENCE           PIC X(48).                   
        01  HYPHEN-LINE.                                                 
            02 FILLER  PIC IS X VALUE IS SPACE.                          
            02 FILLER  PIC IS X(65)    VALUE IS "*****************************************************************".                 
            02 FILLER  PIC IS X(54)    VALUE IS "******************************************************".                            
        01  CCVS-PGM-ID                     PIC X(9)   VALUE             
            "SM202A".                                                    
        PROCEDURE DIVISION.                                              
        CCVS1 SECTION.                                                   
        OPEN-FILES.                                                      
            OPEN     OUTPUT PRINT-FILE.                                  
            MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   
            MOVE    SPACE TO TEST-RESULTS.                               
            PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             
            GO TO CCVS1-EXIT.                                            
        CLOSE-FILES.                                                     
            PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   
        TERMINATE-CCVS.                                                  
            EXIT PROGRAM.                                                
        TERMINATE-CALL.                                                  
            STOP     RUN.                                                
        INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         
        PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           
        FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          
        DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      
            MOVE "****TEST DELETED****" TO RE-MARK.                      
        PRINT-DETAIL.                                                    
            IF REC-CT NOT EQUAL TO ZERO                                  
                    MOVE "." TO PARDOT-X                                 
                    MOVE REC-CT TO DOTVALUE.                             
            MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      
            IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               
               PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 
                 ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 
            MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              
            MOVE SPACE TO CORRECT-X.                                     
            IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         
            MOVE     SPACE TO RE-MARK.                                   
        HEAD-ROUTINE.                                                    
            MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  
            MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  
            MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  
            MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  
        COLUMN-NAMES-ROUTINE.                                            
            MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           
            MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   
            MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        
        END-ROUTINE.                                                     
            MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.
        END-RTN-EXIT.                                                    
            MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   
        END-ROUTINE-1.                                                   
             ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      
             ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               
             ADD PASS-COUNTER TO ERROR-HOLD.                             
             MOVE PASS-COUNTER TO CCVS-E-4-1.                            
             MOVE ERROR-HOLD TO CCVS-E-4-2.                              
             MOVE CCVS-E-4 TO CCVS-E-2-2.                                
             MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           
         END-ROUTINE-12.                                                 
             MOVE "TEST(S) FAILED" TO ENDER-DESC.                        
            IF       ERROR-COUNTER IS EQUAL TO ZERO                      
                MOVE "NO " TO ERROR-TOTAL                                
                ELSE                                                     
                MOVE ERROR-COUNTER TO ERROR-TOTAL.                       
            MOVE     CCVS-E-2 TO DUMMY-RECORD.                           
            PERFORM WRITE-LINE.                                          
        END-ROUTINE-13.                                                  
            IF DELETE-COUNTER IS EQUAL TO ZERO                           
                MOVE "NO " TO ERROR-TOTAL  ELSE                          
                MOVE DELETE-COUNTER TO ERROR-TOTAL.                      
            MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   
            MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           
             IF   INSPECT-COUNTER EQUAL TO ZERO                          
                 MOVE "NO " TO ERROR-TOTAL                               
             ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   
             MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            
             MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          
            MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           
        WRITE-LINE.                                                      
            ADD 1 TO RECORD-COUNT.                                       
            IF RECORD-COUNT GREATER 50                                   
                MOVE DUMMY-RECORD TO DUMMY-HOLD                          
                MOVE SPACE TO DUMMY-RECORD                               
                WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  
                MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             
                MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     
                MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          
                MOVE DUMMY-HOLD TO DUMMY-RECORD                          
                MOVE ZERO TO RECORD-COUNT.                               
            PERFORM WRT-LN.                                              
        WRT-LN.                                                          
            WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               
            MOVE SPACE TO DUMMY-RECORD.                                  
        BLANK-LINE-PRINT.                                                
            PERFORM WRT-LN.                                              
        FAIL-ROUTINE.                                                    
            IF   COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE. 
            IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.
            MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 
            MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   
            MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   
            MOVE   SPACES TO INF-ANSI-REFERENCE.                         
            GO TO  FAIL-ROUTINE-EX.                                      
        FAIL-ROUTINE-WRITE.                                              
            MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         
            MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 
            MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. 
            MOVE   SPACES TO COR-ANSI-REFERENCE.                         
        FAIL-ROUTINE-EX. EXIT.                                           
        BAIL-OUT.                                                        
            IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   
            IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           
        BAIL-OUT-WRITE.                                                  
            MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  
            MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 
            MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   
            MOVE   SPACES TO INF-ANSI-REFERENCE.                         
        BAIL-OUT-EX. EXIT.                                               
        CCVS1-EXIT.                                                      
            EXIT.                                                        
        INITIALIZATION SECTION.                                          
        SM202A-INIT.                                                     
            OPEN     INPUT TEST-FILE.                                    
            MOVE     "SM202A CHECKS A FILE WHICH" TO RE-MARK.            
            PERFORM  PRINT-DETAIL.                                       
            MOVE     "WAS GENERATED IN SM201A."   TO RE-MARK.            
            PERFORM  PRINT-DETAIL.                                       
            MOVE     "COPY FD REPLACING" TO FEATURE.                     
        FD-TEST SECTION.                                                 
        COPY-TEST-11.                                                    
            PERFORM  READ-TEST-FILE.                                     
            IF       TF-1 EQUAL TO 97532                                 
                     PERFORM PASS GO TO COPY-WRITE-11.                   
            GO       TO COPY-FAIL-11.                                    
        COPY-DELETE-11.                                                  
            PERFORM  DE-LETE.                                            
            GO       TO COPY-WRITE-11.                                   
        COPY-FAIL-11.                                                    
            MOVE     TF-1 TO COMPUTED-N.                                 
            MOVE     97532 TO CORRECT-N.                                 
            PERFORM  FAIL.                                               
        COPY-WRITE-11.                                                   
            MOVE     "COPY-TEST-11 " TO PAR-NAME.                        
            PERFORM  PRINT-DETAIL.                                       
        COPY-TEST-12.                                                    
            PERFORM  READ-TEST-FILE.                                     
            IF       TF-1 EQUAL TO 23479                                 
                     PERFORM PASS GO TO COPY-WRITE-12.                   
            GO       TO COPY-FAIL-12.                                    
        COPY-DELETE-12.                                                  
            PERFORM  DE-LETE.                                            
            GO       TO COPY-WRITE-12.                                   
        COPY-FAIL-12.                                                    
            MOVE     TF-1 TO COMPUTED-N.                                 
            MOVE     23479 TO CORRECT-N.                                 
            PERFORM  FAIL.                                               
        COPY-WRITE-12.                                                   
            MOVE     "COPY-TEST-12 " TO PAR-NAME.                        
            PERFORM  PRINT-DETAIL.                                       
        COPY-TEST-13.                                                    
            PERFORM  READ-TEST-FILE 3 TIMES.                             
            IF       TF-1 EQUAL TO 14003                                 
                     PERFORM PASS GO TO COPY-WRITE-13.                   
            GO       TO COPY-FAIL-13.                                    
        COPY-DELETE-13.                                                  
            PERFORM  DE-LETE.                                            
            GO       TO COPY-WRITE-13.                                   
        COPY-FAIL-13.                                                    
            MOVE     TF-1 TO COMPUTED-N.                                 
            MOVE     14003 TO CORRECT-N.                                 
            PERFORM  FAIL.                                               
        COPY-WRITE-13.                                                   
            MOVE     "COPY-TEST-13 " TO PAR-NAME.                        
            PERFORM  PRINT-DETAIL.                                       
        COPY-TEST-14.                                                    
            PERFORM  READ-TEST-FILE 2 TIMES.                             
            IF       TF-1 EQUAL TO 03543                                 
                     PERFORM PASS GO TO COPY-WRITE-14.                   
            GO       TO COPY-FAIL-14.                                    
        COPY-DELETE-14.                                                  
            PERFORM  DE-LETE.                                            
            GO       TO COPY-WRITE-14.                                   
        COPY-FAIL-14.                                                    
            MOVE     TF-1 TO COMPUTED-N.                                 
            MOVE     03543 TO CORRECT-N.                                 
            PERFORM  FAIL.                                               
        COPY-WRITE-14.                                                   
            MOVE     "COPY-TEST-14 " TO PAR-NAME.                        
            PERFORM  PRINT-DETAIL.                                       
        COPY-INIT-A.                                                     
            MOVE     "COPY REPLACING ---  " TO FEATURE.                  
            PERFORM  PRINT-DETAIL.                                       
            MOVE     "  PARAGRAPH-NAMES   " TO FEATURE.                  
        COPY-TEST-15 SECTION.                                            
        PARA-1.                                                          
            ALTER    PARA-2 TO PROCEED TO PARA-4.                        
        PARA-2.                                                          
            GO       TO               PARA-3.                                           
            PARA-3.  PERFORM                                             
            FAIL     .GOTOCOPY-WRITE-15                                  
        .PARA-4                                                          
            .PERFORM        PASS.                                         
            GO       TO COPY-WRITE-15.                                   
        COPY-A-15 SECTION.                                               
        COPY-DELETE-15.                                                  
            PERFORM  DE-LETE.                                            
        COPY-WRITE-15.                                                   
            MOVE     "COPY-TEST-15" TO PAR-NAME.                         
            PERFORM  PRINT-DETAIL.                                       
        COPY-PARA SECTION.                                               
        COPY-INIT-B.                                                     
            MOVE     "  BY LITERALS      " TO FEATURE.                   
        COPY-TEST-16.                                                    
            MOVE          "TRUE " TO AREA-1.                             
            MOVE          " TWO${'$'}" TO AREA-2.                             
            MOVE          "+ 2 =" TO AREA-3.                             
            MOVE                4 TO AREA-4.                             
            IF       TOTAL-AREA EQUAL TO "TRUE  TWO + 2 =    4"          
                     PERFORM PASS ELSE PERFORM FAIL.                     
            GO       TO COPY-WRITE-16.                                   
        COPY-DELETE-16.                                                  
            PERFORM  DE-LETE.                                            
        COPY-WRITE-16.                                                   
            IF COUNTER-16 IS EQUAL TO 0                                  
                PERFORM FAIL                                             
                GO TO COPY-WRITE-17                                      
            ELSE                                                         
                SUBTRACT 1 FROM COUNTER-16.                              
            IF       P-OR-F EQUAL TO "FAIL*"                             
                     MOVE TOTAL-AREA TO COMPUTED-A                       
                     MOVE "TRUE  TWO + 2 =    4" TO CORRECT-A.           
            MOVE     "COPY-TEST-16" TO PAR-NAME.                         
            PERFORM  PRINT-DETAIL.                                       
        COPY-INIT-17.                                                    
            MOVE     SPACE TO TOTAL-AREA.                                
        COPY-TEST-17.                                                    
            MOVE        TRUE-Q-04                                        
         OF TRUE-Q-03                                                 IN TRUE-Q-02
         TO AREA-1.                             
            MOVE     " TWO FIVE " TO AREA-2.                             
            MOVE                Z                                        
         (2, 1, 1)
         TO AREA-3.                             
            MOVE       +000004.99 TO AREA-4.                             
            IF       TOTAL-AREA EQUAL TO "TRUE  TWO + 2 =    4"          
                     PERFORM PASS ELSE PERFORM FAIL.                     
            GO       TO COPY-WRITE-17.                                   
        COPY-DELETE-17.                                                  
            PERFORM  DE-LETE.                                            
        COPY-WRITE-17.                                                   
            IF       P-OR-F EQUAL TO "FAIL*"                             
                     MOVE TOTAL-AREA TO COMPUTED-A                       
                     MOVE "TRUE  TWO + 2 =    4" TO CORRECT-A.           
            MOVE     "COPY-TEST-17" TO PAR-NAME.                         
            PERFORM  PRINT-DETAIL.                                       
            CLOSE    TEST-FILE.                                          
            GO TO CCVS-EXIT.                                             
        READ-TEST-FILE.                                                  
            READ     TEST-FILE          AT END GO TO BAD-FILE.           
        BAD-FILE.                                                        
            PERFORM  FAIL.                                               
            MOVE     "BAD-FILE" TO PAR-NAME.                             
            MOVE     "EOF PREMATURELY FOUND" TO RE-MARK.                 
            PERFORM  PRINT-DETAIL.                                       
            CLOSE    TEST-FILE.                                          
            GO TO CCVS-EXIT.                                             
        CCVS-EXIT SECTION.                                               
        CCVS-999999.                                                     
            GO TO CLOSE-FILES.                                           
        """.trimIndent()

    val sm203A =
        """
        IDENTIFICATION DIVISION.                                         
        PROGRAM-ID.                                                      
            SM203A.                                                      
        ENVIRONMENT DIVISION.                                            
        CONFIGURATION SECTION.                                           
        SOURCE-COMPUTER.                                                 
            XXXXX082.                                                    
        OBJECT-COMPUTER.                                                 
            XXXXX083.                                                    
        SPECIAL-NAMES.    XXXXX051                                                     
            IS       SW-1                                                
            ON STATUS IS                SWITCH-ON                                        
            OFF STATUS IS                 SWITCH-OFF.                                     
        INPUT-OUTPUT SECTION.                                            
        FILE-CONTROL.    SELECT PRINT-FILE ASSIGN TO                                  
            XXXXX055.                                                    
            SELECT       TEST-FILE ASSIGN TO                             
            XXXXP002.                                                    
        I-O-CONTROL.    SAME RECORD AREA FOR TEST-FILE      , PRINT-FILE.            
        DATA DIVISION.                                                   
        FILE SECTION.                                                    
        FD  PRINT-FILE.                                                  
        01  PRINT-REC PICTURE X(120).                                    
        01  DUMMY-RECORD PICTURE X(120).                                 
        FD  TEST-FILE                                                    
            LABEL RECORD STANDARD                                        
            VALUE OF                                                     
            XXXXX074                                                     
            IS                                                           
            XXXXX077                                                     
            DATA RECORD IS PROOF-REC.                                    
        01  PROOF-REC.                                                   
            02  TF-1 PICTURE 9(5).                                       
            02  FILLER PICTURE X(115).                                   
        WORKING-STORAGE SECTION.                                         
        77  RCD-1 PICTURE 9(5) VALUE 97532.                              
        77  RCD-2 PICTURE 9(5) VALUE 23479.                              
        77  RCD-3 PICTURE 9(5) VALUE 10901.                              
        77  RCD-4 PICTURE 9(5) VALUE 02734.                              
        77  RCD-5 PICTURE 9(5) VALUE 14003.                              
        77  RCD-6 PICTURE 9(5) VALUE 19922.                              
        77  RCD-7 PICTURE 9(5) VALUE 03543.                              
        01  TEST-RESULTS.                                                
            02 FILLER                   PIC X      VALUE SPACE.          
            02 FEATURE                  PIC X(20)  VALUE SPACE.          
            02 FILLER                   PIC X      VALUE SPACE.          
            02 P-OR-F                   PIC X(5)   VALUE SPACE.          
            02 FILLER                   PIC X      VALUE SPACE.          
            02  PAR-NAME.                                                
              03 FILLER                 PIC X(19)  VALUE SPACE.          
              03  PARDOT-X              PIC X      VALUE SPACE.          
              03 DOTVALUE               PIC 99     VALUE ZERO.           
            02 FILLER                   PIC X(8)   VALUE SPACE.          
            02 RE-MARK                  PIC X(61).                       
        01  TEST-COMPUTED.                                               
            02 FILLER                   PIC X(30)  VALUE SPACE.          
            02 FILLER                   PIC X(17)  VALUE                 
                   "       COMPUTED=".                                   
            02 COMPUTED-X.                                               
            03 COMPUTED-A               PIC X(20)  VALUE SPACE.          
            03 COMPUTED-N               REDEFINES COMPUTED-A             
                                        PIC -9(9).9(9).                  
            03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         
            03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     
            03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     
            03       CM-18V0 REDEFINES COMPUTED-A.                       
                04 COMPUTED-18V0                    PIC -9(18).          
                04 FILLER                           PIC X.               
            03 FILLER PIC X(50) VALUE SPACE.                             
        01  TEST-CORRECT.                                                
            02 FILLER PIC X(30) VALUE SPACE.                             
            02 FILLER PIC X(17) VALUE "       CORRECT =".                
            02 CORRECT-X.                                                
            03 CORRECT-A                  PIC X(20) VALUE SPACE.         
            03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      
            03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         
            03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     
            03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     
            03      CR-18V0 REDEFINES CORRECT-A.                         
                04 CORRECT-18V0                     PIC -9(18).          
                04 FILLER                           PIC X.               
            03 FILLER PIC X(2) VALUE SPACE.                              
            03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     
        01  CCVS-C-1.                                                    
            02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PASS  PARAGRAPH-NAME                                                 REMARKS".                                            
            02 FILLER                     PIC X(20)    VALUE SPACE.      
        01  CCVS-C-2.                                                    
            02 FILLER                     PIC X        VALUE SPACE.      
            02 FILLER                     PIC X(6)     VALUE "TESTED".   
            02 FILLER                     PIC X(15)    VALUE SPACE.      
            02 FILLER                     PIC X(4)     VALUE "FAIL".     
            02 FILLER                     PIC X(94)    VALUE SPACE.      
        01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       
        01  REC-CT                        PIC 99       VALUE ZERO.       
        01  DELETE-COUNTER                PIC 999      VALUE ZERO.       
        01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       
        01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       
        01  PASS-COUNTER                  PIC 999      VALUE ZERO.       
        01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       
        01  ERROR-HOLD                    PIC 999      VALUE ZERO.       
        01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      
        01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       
        01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     
        01  CCVS-H-1.                                                    
            02  FILLER                    PIC X(39)    VALUE SPACES.     
            02  FILLER                    PIC X(42)    VALUE             
            "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 
            02  FILLER                    PIC X(39)    VALUE SPACES.     
        01  CCVS-H-2A.                                                   
          02  FILLER                        PIC X(40)  VALUE SPACE.      
          02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  
          02  FILLER                        PIC XXXX   VALUE             
            "4.2 ".                                                      
          02  FILLER                        PIC X(28)  VALUE             
                   " COPY - NOT FOR DISTRIBUTION".                       
          02  FILLER                        PIC X(41)  VALUE SPACE.      
        01  CCVS-H-2B.                                                   
          02  FILLER                        PIC X(15)  VALUE             
                   "TEST RESULT OF ".                                    
          02  TEST-ID                       PIC X(9).                    
          02  FILLER                        PIC X(4)   VALUE             
                   " IN ".                                               
          02  FILLER                        PIC X(12)  VALUE             
            " HIGH       ".                                              
          02  FILLER                        PIC X(22)  VALUE             
                   " LEVEL VALIDATION FOR ".                             
          02  FILLER                        PIC X(58)  VALUE             
            "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".
        01  CCVS-H-3.                                                    
            02  FILLER                      PIC X(34)  VALUE             
                   " FOR OFFICIAL USE ONLY    ".                         
            02  FILLER                      PIC X(58)  VALUE             
            "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".
            02  FILLER                      PIC X(28)  VALUE             
                   "  COPYRIGHT   1985 ".                                
        01  CCVS-E-1.                                                    
            02 FILLER                       PIC X(52)  VALUE SPACE.      
            02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              
            02 ID-AGAIN                     PIC X(9).                    
            02 FILLER                       PIC X(45)  VALUE SPACES.     
        01  CCVS-E-2.                                                    
            02  FILLER                      PIC X(31)  VALUE SPACE.      
            02  FILLER                      PIC X(21)  VALUE SPACE.      
            02 CCVS-E-2-2.                                               
                03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      
                03 FILLER                   PIC X      VALUE SPACE.      
                03 ENDER-DESC               PIC X(44)  VALUE             
                   "ERRORS ENCOUNTERED".                                 
        01  CCVS-E-3.                                                    
            02  FILLER                      PIC X(22)  VALUE             
                   " FOR OFFICIAL USE ONLY".                             
            02  FILLER                      PIC X(12)  VALUE SPACE.      
            02  FILLER                      PIC X(58)  VALUE             
            "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".
            02  FILLER                      PIC X(13)  VALUE SPACE.      
            02 FILLER                       PIC X(15)  VALUE             
                    " COPYRIGHT 1985".                                   
        01  CCVS-E-4.                                                    
            02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      
            02 FILLER                       PIC X(4)   VALUE " OF ".     
            02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      
            02 FILLER                       PIC X(40)  VALUE             
             "  TESTS WERE EXECUTED SUCCESSFULLY".                       
        01  XXINFO.                                                      
            02 FILLER                       PIC X(19)  VALUE             
                   "*** INFORMATION ***".                                
            02 INFO-TEXT.                                                
              04 FILLER                     PIC X(8)   VALUE SPACE.      
              04 XXCOMPUTED                 PIC X(20).                   
              04 FILLER                     PIC X(5)   VALUE SPACE.      
              04 XXCORRECT                  PIC X(20).                   
            02 INF-ANSI-REFERENCE           PIC X(48).                   
        01  HYPHEN-LINE.                                                 
            02 FILLER  PIC IS X VALUE IS SPACE.                          
            02 FILLER  PIC IS X(65)    VALUE IS "*****************************************************************".                 
            02 FILLER  PIC IS X(54)    VALUE IS "******************************************************".                            
        01  CCVS-PGM-ID                     PIC X(9)   VALUE             
            "SM203A".                                                    
        PROCEDURE DIVISION.                                              
        CCVS1 SECTION.                                                   
        OPEN-FILES.                                                      
            OPEN     OUTPUT PRINT-FILE.                                  
            MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   
            MOVE    SPACE TO TEST-RESULTS.                               
            PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             
            GO TO CCVS1-EXIT.                                            
        CLOSE-FILES.                                                     
            PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   
        TERMINATE-CCVS.                                                  
            EXIT PROGRAM.                                                
        TERMINATE-CALL.                                                  
            STOP     RUN.                                                
        INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         
        PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           
        FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          
        DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      
            MOVE "****TEST DELETED****" TO RE-MARK.                      
        PRINT-DETAIL.                                                    
            IF REC-CT NOT EQUAL TO ZERO                                  
                    MOVE "." TO PARDOT-X                                 
                    MOVE REC-CT TO DOTVALUE.                             
            MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      
            IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               
               PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 
                 ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 
            MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              
            MOVE SPACE TO CORRECT-X.                                     
            IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         
            MOVE     SPACE TO RE-MARK.                                   
        HEAD-ROUTINE.                                                    
            MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  
            MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  
            MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  
            MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  
        COLUMN-NAMES-ROUTINE.                                            
            MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           
            MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   
            MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        
        END-ROUTINE.                                                     
            MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.
        END-RTN-EXIT.                                                    
            MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   
        END-ROUTINE-1.                                                   
             ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      
             ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               
             ADD PASS-COUNTER TO ERROR-HOLD.                             
             MOVE PASS-COUNTER TO CCVS-E-4-1.                            
             MOVE ERROR-HOLD TO CCVS-E-4-2.                              
             MOVE CCVS-E-4 TO CCVS-E-2-2.                                
             MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           
         END-ROUTINE-12.                                                 
             MOVE "TEST(S) FAILED" TO ENDER-DESC.                        
            IF       ERROR-COUNTER IS EQUAL TO ZERO                      
                MOVE "NO " TO ERROR-TOTAL                                
                ELSE                                                     
                MOVE ERROR-COUNTER TO ERROR-TOTAL.                       
            MOVE     CCVS-E-2 TO DUMMY-RECORD.                           
            PERFORM WRITE-LINE.                                          
        END-ROUTINE-13.                                                  
            IF DELETE-COUNTER IS EQUAL TO ZERO                           
                MOVE "NO " TO ERROR-TOTAL  ELSE                          
                MOVE DELETE-COUNTER TO ERROR-TOTAL.                      
            MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   
            MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           
             IF   INSPECT-COUNTER EQUAL TO ZERO                          
                 MOVE "NO " TO ERROR-TOTAL                               
             ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   
             MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            
             MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          
            MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           
        WRITE-LINE.                                                      
            ADD 1 TO RECORD-COUNT.                                       
            IF RECORD-COUNT GREATER 50                                   
                MOVE DUMMY-RECORD TO DUMMY-HOLD                          
                MOVE SPACE TO DUMMY-RECORD                               
                WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  
                MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             
                MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     
                MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          
                MOVE DUMMY-HOLD TO DUMMY-RECORD                          
                MOVE ZERO TO RECORD-COUNT.                               
            PERFORM WRT-LN.                                              
        WRT-LN.                                                          
            WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               
            MOVE SPACE TO DUMMY-RECORD.                                  
        BLANK-LINE-PRINT.                                                
            PERFORM WRT-LN.                                              
        FAIL-ROUTINE.                                                    
            IF   COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE. 
            IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.
            MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 
            MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   
            MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   
            MOVE   SPACES TO INF-ANSI-REFERENCE.                         
            GO TO  FAIL-ROUTINE-EX.                                      
        FAIL-ROUTINE-WRITE.                                              
            MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         
            MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 
            MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. 
            MOVE   SPACES TO COR-ANSI-REFERENCE.                         
        FAIL-ROUTINE-EX. EXIT.                                           
        BAIL-OUT.                                                        
            IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   
            IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           
        BAIL-OUT-WRITE.                                                  
            MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  
            MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 
            MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   
            MOVE   SPACES TO INF-ANSI-REFERENCE.                         
        BAIL-OUT-EX. EXIT.                                               
        CCVS1-EXIT.                                                      
            EXIT.                                                        
        INITIALIZATION SECTION.                                          
        SM203-INIT.                                                      
            OPEN     OUTPUT TEST-FILE.                                   
        BUILD SECTION.                                                   
        COPY-TEST-1.                                                     
            MOVE     RCD-1 TO TF-1.                                      
            WRITE    PROOF-REC.                                          
            MOVE     RCD-2 TO TF-1.                                      
            WRITE    PROOF-REC.                                          
            MOVE     RCD-3 TO TF-1.                                      
            WRITE    PROOF-REC.                                          
            MOVE     RCD-4 TO TF-1.                                      
            WRITE    PROOF-REC.                                          
            MOVE     RCD-5 TO TF-1.                                      
            WRITE    PROOF-REC.                                          
            MOVE     RCD-6 TO TF-1.                                      
            WRITE    PROOF-REC.                                          
            MOVE     RCD-7 TO TF-1.                                      
            WRITE    PROOF-REC.                                          
            PERFORM  PASS.                                               
            GO       TO COPY-WRITE-1.                                    
        COPY-DELETE-1.                                                   
            PERFORM  DE-LETE.                                            
        COPY-WRITE-1.                                                    
            MOVE     "COPY ENV DIV REPLAC" TO FEATURE.                   
            MOVE     "COPY-TEST-1 " TO PAR-NAME.                         
            PERFORM  PRINT-DETAIL.                                       
            CLOSE    TEST-FILE.                                          
        CCVS-EXIT SECTION.                                               
        CCVS-999999.                                                     
            GO TO CLOSE-FILES.                                           
        """.trimIndent()

    val sm205A =
        """
        IDENTIFICATION DIVISION.                                         
        PROGRAM-ID.                                                      
            SM205A.                                                      
        ENVIRONMENT DIVISION.                                            
        CONFIGURATION SECTION.                                           
        SOURCE-COMPUTER.                                                 
            XXXXX082.                                                    
        OBJECT-COMPUTER.                                                 
            XXXXX083.                                                    
        INPUT-OUTPUT SECTION.                                            
        FILE-CONTROL.                                                    
            SELECT PRINT-FILE ASSIGN TO                                  
            XXXXX055.                                                    
            SELECT SORTFILE-2E ASSIGN TO                                 
            XXXXX027.                                                    
            SELECT SORTOUT-2E ASSIGN TO                                  
            XXXXX001.                                                    
        DATA DIVISION.                                                   
        FILE SECTION.                                                    
        FD  PRINT-FILE.                                                  
        01  PRINT-REC PICTURE X(120).                                    
        01  DUMMY-RECORD PICTURE X(120).                                 
        SD  SORTFILE-2E    DATA RECORD S-RECORD.                                        
        01  S-RECORD.    02  KEYS-GROUP.                                              
                03  KEY-1 PICTURE 9.                                     
                03  KEY-2 PICTURE 99.                                    
                03  KEY-3 PICTURE 999.                                   
                03  KEY-4 PICTURE 9999.                                  
                03  KEY-5 PICTURE 99999.                                 
            02 RDF-KEYS REDEFINES KEYS-GROUP PICTURE 9(15).              
            02 FILLER PICTURE X(105).                                    
        FD  SORTOUT-2E                                                   
            BLOCK CONTAINS 10 RECORDS                                    
            LABEL RECORDS ARE STANDARD                                   
            VALUE OF                                                     
            XXXXX074                                                     
            IS                                                           
            XXXXX076                                                     
            DATA RECORD SORTED.                                          
        01  SORTED PICTURE X(120).                                       
        WORKING-STORAGE SECTION.                                         
        77  C0 PICTURE 9 VALUE 0.                                        
        77  C1 PICTURE 9 VALUE 1.                                        
        77  C2 PICTURE 9 VALUE 2.                                        
        77  C6 PICTURE 9 VALUE 6.                                        
        77  C3 PICTURE 9 VALUE 3.                                        
        01  WKEYS-GROUP.                                                 
            02  WKEY-1  PICTURE 9.                                       
            02  WKEY-2  PICTURE 99.                                      
            02  WKEY-3  PICTURE 999.                                     
            02  WKEY-4  PICTURE 9999.                                    
            02  WKEY-5 PICTURE 9(5).                                     
        01  WKEYS-RDF REDEFINES WKEYS-GROUP PICTURE 9(15).               
        01  TEST-RESULTS.                                                
            02 FILLER                   PIC X      VALUE SPACE.          
            02 FEATURE                  PIC X(20)  VALUE SPACE.          
            02 FILLER                   PIC X      VALUE SPACE.          
            02 P-OR-F                   PIC X(5)   VALUE SPACE.          
            02 FILLER                   PIC X      VALUE SPACE.          
            02  PAR-NAME.                                                
              03 FILLER                 PIC X(19)  VALUE SPACE.          
              03  PARDOT-X              PIC X      VALUE SPACE.          
              03 DOTVALUE               PIC 99     VALUE ZERO.           
            02 FILLER                   PIC X(8)   VALUE SPACE.          
            02 RE-MARK                  PIC X(61).                       
        01  TEST-COMPUTED.                                               
            02 FILLER                   PIC X(30)  VALUE SPACE.          
            02 FILLER                   PIC X(17)  VALUE                 
                   "       COMPUTED=".                                   
            02 COMPUTED-X.                                               
            03 COMPUTED-A               PIC X(20)  VALUE SPACE.          
            03 COMPUTED-N               REDEFINES COMPUTED-A             
                                        PIC -9(9).9(9).                  
            03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         
            03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     
            03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     
            03       CM-18V0 REDEFINES COMPUTED-A.                       
                04 COMPUTED-18V0                    PIC -9(18).          
                04 FILLER                           PIC X.               
            03 FILLER PIC X(50) VALUE SPACE.                             
        01  TEST-CORRECT.                                                
            02 FILLER PIC X(30) VALUE SPACE.                             
            02 FILLER PIC X(17) VALUE "       CORRECT =".                
            02 CORRECT-X.                                                
            03 CORRECT-A                  PIC X(20) VALUE SPACE.         
            03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      
            03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         
            03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     
            03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     
            03      CR-18V0 REDEFINES CORRECT-A.                         
                04 CORRECT-18V0                     PIC -9(18).          
                04 FILLER                           PIC X.               
            03 FILLER PIC X(2) VALUE SPACE.                              
            03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     
        01  CCVS-C-1.                                                    
            02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PASS  PARAGRAPH-NAME                                                 REMARKS".                                            
            02 FILLER                     PIC X(20)    VALUE SPACE.      
        01  CCVS-C-2.                                                    
            02 FILLER                     PIC X        VALUE SPACE.      
            02 FILLER                     PIC X(6)     VALUE "TESTED".   
            02 FILLER                     PIC X(15)    VALUE SPACE.      
            02 FILLER                     PIC X(4)     VALUE "FAIL".     
            02 FILLER                     PIC X(94)    VALUE SPACE.      
        01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       
        01  REC-CT                        PIC 99       VALUE ZERO.       
        01  DELETE-COUNTER                PIC 999      VALUE ZERO.       
        01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       
        01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       
        01  PASS-COUNTER                  PIC 999      VALUE ZERO.       
        01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       
        01  ERROR-HOLD                    PIC 999      VALUE ZERO.       
        01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      
        01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       
        01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     
        01  CCVS-H-1.                                                    
            02  FILLER                    PIC X(39)    VALUE SPACES.     
            02  FILLER                    PIC X(42)    VALUE             
            "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 
            02  FILLER                    PIC X(39)    VALUE SPACES.     
        01  CCVS-H-2A.                                                   
          02  FILLER                        PIC X(40)  VALUE SPACE.      
          02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  
          02  FILLER                        PIC XXXX   VALUE             
            "4.2 ".                                                      
          02  FILLER                        PIC X(28)  VALUE             
                   " COPY - NOT FOR DISTRIBUTION".                       
          02  FILLER                        PIC X(41)  VALUE SPACE.      
        01  CCVS-H-2B.                                                   
          02  FILLER                        PIC X(15)  VALUE             
                   "TEST RESULT OF ".                                    
          02  TEST-ID                       PIC X(9).                    
          02  FILLER                        PIC X(4)   VALUE             
                   " IN ".                                               
          02  FILLER                        PIC X(12)  VALUE             
            " HIGH       ".                                              
          02  FILLER                        PIC X(22)  VALUE             
                   " LEVEL VALIDATION FOR ".                             
          02  FILLER                        PIC X(58)  VALUE             
            "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".
        01  CCVS-H-3.                                                    
            02  FILLER                      PIC X(34)  VALUE             
                   " FOR OFFICIAL USE ONLY    ".                         
            02  FILLER                      PIC X(58)  VALUE             
            "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".
            02  FILLER                      PIC X(28)  VALUE             
                   "  COPYRIGHT   1985 ".                                
        01  CCVS-E-1.                                                    
            02 FILLER                       PIC X(52)  VALUE SPACE.      
            02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              
            02 ID-AGAIN                     PIC X(9).                    
            02 FILLER                       PIC X(45)  VALUE SPACES.     
        01  CCVS-E-2.                                                    
            02  FILLER                      PIC X(31)  VALUE SPACE.      
            02  FILLER                      PIC X(21)  VALUE SPACE.      
            02 CCVS-E-2-2.                                               
                03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      
                03 FILLER                   PIC X      VALUE SPACE.      
                03 ENDER-DESC               PIC X(44)  VALUE             
                   "ERRORS ENCOUNTERED".                                 
        01  CCVS-E-3.                                                    
            02  FILLER                      PIC X(22)  VALUE             
                   " FOR OFFICIAL USE ONLY".                             
            02  FILLER                      PIC X(12)  VALUE SPACE.      
            02  FILLER                      PIC X(58)  VALUE             
            "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".
            02  FILLER                      PIC X(13)  VALUE SPACE.      
            02 FILLER                       PIC X(15)  VALUE             
                    " COPYRIGHT 1985".                                   
        01  CCVS-E-4.                                                    
            02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      
            02 FILLER                       PIC X(4)   VALUE " OF ".     
            02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      
            02 FILLER                       PIC X(40)  VALUE             
             "  TESTS WERE EXECUTED SUCCESSFULLY".                       
        01  XXINFO.                                                      
            02 FILLER                       PIC X(19)  VALUE             
                   "*** INFORMATION ***".                                
            02 INFO-TEXT.                                                
              04 FILLER                     PIC X(8)   VALUE SPACE.      
              04 XXCOMPUTED                 PIC X(20).                   
              04 FILLER                     PIC X(5)   VALUE SPACE.      
              04 XXCORRECT                  PIC X(20).                   
            02 INF-ANSI-REFERENCE           PIC X(48).                   
        01  HYPHEN-LINE.                                                 
            02 FILLER  PIC IS X VALUE IS SPACE.                          
            02 FILLER  PIC IS X(65)    VALUE IS "*****************************************************************".                 
            02 FILLER  PIC IS X(54)    VALUE IS "******************************************************".                            
        01  CCVS-PGM-ID                     PIC X(9)   VALUE             
            "SM205A".                                                    
        PROCEDURE DIVISION.                                              
        CCVS1 SECTION.                                                   
        OPEN-FILES.                                                      
            OPEN     OUTPUT PRINT-FILE.                                  
            MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   
            MOVE    SPACE TO TEST-RESULTS.                               
            PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             
            GO TO CCVS1-EXIT.                                            
        CLOSE-FILES.                                                     
            PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   
        TERMINATE-CCVS.                                                  
            EXIT PROGRAM.                                                
        TERMINATE-CALL.                                                  
            STOP     RUN.                                                
        INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         
        PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           
        FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          
        DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      
            MOVE "****TEST DELETED****" TO RE-MARK.                      
        PRINT-DETAIL.                                                    
            IF REC-CT NOT EQUAL TO ZERO                                  
                    MOVE "." TO PARDOT-X                                 
                    MOVE REC-CT TO DOTVALUE.                             
            MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      
            IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               
               PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 
                 ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 
            MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              
            MOVE SPACE TO CORRECT-X.                                     
            IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         
            MOVE     SPACE TO RE-MARK.                                   
        HEAD-ROUTINE.                                                    
            MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  
            MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  
            MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  
            MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  
        COLUMN-NAMES-ROUTINE.                                            
            MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           
            MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   
            MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        
        END-ROUTINE.                                                     
            MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.
        END-RTN-EXIT.                                                    
            MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   
        END-ROUTINE-1.                                                   
             ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      
             ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               
             ADD PASS-COUNTER TO ERROR-HOLD.                             
             MOVE PASS-COUNTER TO CCVS-E-4-1.                            
             MOVE ERROR-HOLD TO CCVS-E-4-2.                              
             MOVE CCVS-E-4 TO CCVS-E-2-2.                                
             MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           
         END-ROUTINE-12.                                                 
             MOVE "TEST(S) FAILED" TO ENDER-DESC.                        
            IF       ERROR-COUNTER IS EQUAL TO ZERO                      
                MOVE "NO " TO ERROR-TOTAL                                
                ELSE                                                     
                MOVE ERROR-COUNTER TO ERROR-TOTAL.                       
            MOVE     CCVS-E-2 TO DUMMY-RECORD.                           
            PERFORM WRITE-LINE.                                          
        END-ROUTINE-13.                                                  
            IF DELETE-COUNTER IS EQUAL TO ZERO                           
                MOVE "NO " TO ERROR-TOTAL  ELSE                          
                MOVE DELETE-COUNTER TO ERROR-TOTAL.                      
            MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   
            MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           
             IF   INSPECT-COUNTER EQUAL TO ZERO                          
                 MOVE "NO " TO ERROR-TOTAL                               
             ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   
             MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            
             MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          
            MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           
        WRITE-LINE.                                                      
            ADD 1 TO RECORD-COUNT.                                       
            IF RECORD-COUNT GREATER 50                                   
                MOVE DUMMY-RECORD TO DUMMY-HOLD                          
                MOVE SPACE TO DUMMY-RECORD                               
                WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  
                MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             
                MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     
                MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          
                MOVE DUMMY-HOLD TO DUMMY-RECORD                          
                MOVE ZERO TO RECORD-COUNT.                               
            PERFORM WRT-LN.                                              
        WRT-LN.                                                          
            WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               
            MOVE SPACE TO DUMMY-RECORD.                                  
        BLANK-LINE-PRINT.                                                
            PERFORM WRT-LN.                                              
        FAIL-ROUTINE.                                                    
            IF   COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE. 
            IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.
            MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 
            MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   
            MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   
            MOVE   SPACES TO INF-ANSI-REFERENCE.                         
            GO TO  FAIL-ROUTINE-EX.                                      
        FAIL-ROUTINE-WRITE.                                              
            MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         
            MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 
            MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. 
            MOVE   SPACES TO COR-ANSI-REFERENCE.                         
        FAIL-ROUTINE-EX. EXIT.                                           
        BAIL-OUT.                                                        
            IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   
            IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           
        BAIL-OUT-WRITE.                                                  
            MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  
            MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 
            MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   
            MOVE   SPACES TO INF-ANSI-REFERENCE.                         
        BAIL-OUT-EX. EXIT.                                               
        CCVS1-EXIT.                                                      
            EXIT.                                                        
        SORT-INIT SECTION.                                               
        I-1.                                                             
            SORT SORTFILE-2E                                             
            ON ASCENDING KEY KEY-1                                       
            ON DESCENDING KEY KEY-2                                      
            ON ASCENDING KEY KEY-3                                       
            DESCENDING KEY-4 KEY-5                                       
            INPUT PROCEDURE IS INSORT                                    
            OUTPUT PROCEDURE IS OUTP1 THRU OUTP3.                        
        I-2.                                                             
            GO TO    CLOSE-FILES.                                        
        INSORT SECTION.                                                  
        IN-1.                                                            
        IN-2.                                                            
            MOVE 900009000000000 TO RDF-KEYS.                            
            RELEASE S-RECORD.                                            
            MOVE 009000000900009 TO RDF-KEYS.                            
            RELEASE S-RECORD.                                            
            MOVE 900008000000000 TO RDF-KEYS.                            
            RELEASE S-RECORD.                                            
            MOVE 009000000900008 TO RDF-KEYS.                            
            RELEASE S-RECORD.                                            
            MOVE 300003000000000 TO WKEYS-RDF.                           
        IN-3.                                                            
            PERFORM IN-4 2 TIMES.                                        
            GO TO IN-EXIT.                                               
        IN-4.                                                            
            SUBTRACT C1 FROM WKEY-1.                                     
            PERFORM IN-5 6 TIMES.                                        
        IN-5.                                                            
            IF WKEY-2 IS EQUAL TO C6                                     
                MOVE C0 TO WKEY-2.                                       
            ADD C1 TO WKEY-2.                                            
            PERFORM IN-6 2 TIMES.                                        
        IN-6.                                                            
            IF WKEY-3 IS EQUAL TO C1                                     
                MOVE C3 TO WKEY-3.                                       
            SUBTRACT C1 FROM WKEY-3.                                     
            PERFORM IN-7 2 TIMES.                                        
        IN-7.                                                            
            IF       WKEY-4 EQUAL TO C2                                  
                MOVE C0 TO WKEY-4.                                       
            ADD C1 TO WKEY-4.                                            
            PERFORM IN-8 2 TIMES.                                        
        IN-8.                                                            
            IF WKEY-5 IS EQUAL TO C2                                     
                MOVE C0 TO WKEY-5.                                       
            ADD C1 TO WKEY-5.                                            
            MOVE WKEYS-RDF TO RDF-KEYS.                                  
            RELEASE S-RECORD.                                            
        IN-EXIT.                                                         
            EXIT.                                                        
        OUTP1 SECTION.                                                   
        WOUTPT1.                                                         
            OPEN     OUTPUT SORTOUT-2E.                                  
            MOVE     SPACE TO TEST-RESULTS.                              
            MOVE     "COPY SD REPLACING" TO FEATURE.                     
        COPY-TEST-1.                                                     
            PERFORM  RET-1.                                              
            IF       RDF-KEYS EQUAL TO 009000000900009                   
                     PERFORM PASS-1 GO TO COPY-WRITE-1.                  
            GO       TO COPY-FAIL-1-1.                                   
        COPY-DELETE-1.                                                   
            PERFORM  DE-LETE-1.                                          
            GO       TO COPY-WRITE-1.                                    
        COPY-FAIL-1-1.                                                   
            MOVE     RDF-KEYS TO COMPUTED-18V0.                          
            MOVE     009000000900009 TO CORRECT-18V0.                    
            PERFORM  FAIL-1.                                             
        COPY-WRITE-1.                                                    
            MOVE     "COPY-TEST-1 " TO PAR-NAME.                         
            PERFORM  PRINT-DETAIL-1.                                     
        COPY-TEST-2.                                                     
            PERFORM  RET-1.                                              
            IF       RDF-KEYS EQUAL TO 009000000900008                   
                     PERFORM PASS-1 GO TO COPY-WRITE-2.                  
            GO       TO COPY-FAIL-1-2.                                   
        COPY-DELETE-2.                                                   
            PERFORM  DE-LETE-1.                                          
            GO       TO COPY-WRITE-2.                                    
        COPY-FAIL-1-2.                                                   
            MOVE     RDF-KEYS TO COMPUTED-18V0.                          
            MOVE     009000000900008 TO CORRECT-18V0.                    
            PERFORM  FAIL-1.                                             
        COPY-WRITE-2.                                                    
            MOVE     "COPY-TEST-2 " TO PAR-NAME.                         
            PERFORM  PRINT-DETAIL-1.                                     
        COPY-TEST-3.                                                     
            PERFORM  RET-1.                                              
            IF       RDF-KEYS EQUAL TO 106001000200002                   
                     PERFORM PASS-1 GO TO COPY-WRITE-3.                  
            GO       TO COPY-FAIL-1-3.                                   
        COPY-DELETE-3.                                                   
            PERFORM  DE-LETE-1.                                          
            GO       TO COPY-WRITE-3.                                    
        COPY-FAIL-1-3.                                                   
            MOVE     RDF-KEYS TO COMPUTED-18V0.                          
            MOVE     106001000200002 TO CORRECT-18V0.                    
            PERFORM  FAIL-1.                                             
        COPY-WRITE-3.                                                    
            MOVE     "COPY-TEST-3 " TO PAR-NAME.                         
            PERFORM  PRINT-DETAIL-1.                                     
        COPY-TEST-4.                                                     
            PERFORM  RET-2 48 TIMES.                                     
            IF       RDF-KEYS EQUAL TO 206001000200002                   
                     PERFORM PASS-1 GO TO COPY-WRITE-4.                  
            GO       TO COPY-FAIL-1-4.                                   
        COPY-DELETE-4.                                                   
            PERFORM  DE-LETE-1.                                          
            GO       TO COPY-WRITE-4.                                    
        COPY-FAIL-1-4.                                                   
            MOVE     RDF-KEYS TO COMPUTED-18V0.                          
            MOVE     206001000200002 TO CORRECT-18V0.                    
            PERFORM  FAIL-1.                                             
        COPY-WRITE-4.                                                    
            MOVE "COPY-TEST-4 " TO PAR-NAME.                             
            PERFORM  PRINT-DETAIL-1.                                     
        COPY-TEST-5.                                                     
            PERFORM  RET-2 40 TIMES.                                     
            IF       RDF-KEYS EQUAL TO 201001000200002                   
                     PERFORM PASS-1 GO TO COPY-WRITE-5.                  
            GO       TO COPY-FAIL-1-5.                                   
        COPY-DELETE-5.                                                   
            PERFORM  DE-LETE-1.                                          
            GO       TO COPY-WRITE-5.                                    
        COPY-FAIL-1-5.                                                   
            MOVE     RDF-KEYS TO COMPUTED-18V0.                          
            MOVE     201001000200002 TO CORRECT-18V0.                    
            PERFORM  FAIL-1.                                             
        COPY-WRITE-5.                                                    
            MOVE     "COPY-TEST-5 " TO PAR-NAME.                         
            PERFORM  PRINT-DETAIL-1.                                     
        COPY-TEST-6.                                                     
            PERFORM  RET-2  7 TIMES.                                     
            IF       RDF-KEYS EQUAL TO 201002000100001                   
                     PERFORM PASS-1 GO TO COPY-WRITE-6.                  
            GO       TO COPY-FAIL-1-6.                                   
        COPY-DELETE-6.                                                   
            PERFORM  DE-LETE-1.                                          
            GO       TO COPY-WRITE-6.                                    
        COPY-FAIL-1-6.                                                   
            MOVE     RDF-KEYS TO COMPUTED-18V0.                          
            MOVE     201002000100001 TO CORRECT-18V0.                    
            PERFORM  FAIL-1.                                             
        COPY-WRITE-6.                                                    
            MOVE     "COPY-TEST-6 " TO PAR-NAME.                         
            PERFORM  PRINT-DETAIL-1.                                     
        COPY-TEST-7.                                                     
            PERFORM  RET-2.                                              
            IF       RDF-KEYS EQUAL TO 900008000000000                   
                     PERFORM PASS-1 GO TO COPY-WRITE-7.                  
            GO       TO COPY-FAIL-1-7.                                   
        COPY-DELETE-7.                                                   
            PERFORM  DE-LETE-1.                                          
            GO       TO COPY-WRITE-7.                                    
        COPY-FAIL-1-7.                                                   
            MOVE     RDF-KEYS TO COMPUTED-18V0.                          
            MOVE     900008000000000 TO CORRECT-18V0.                    
            PERFORM  FAIL-1.                                             
        COPY-WRITE-7.                                                    
            MOVE     "COPY-TEST-7 " TO PAR-NAME.                         
            PERFORM  PRINT-DETAIL-1.                                     
        COPY-TEST-8.                                                     
            PERFORM  RET-2.                                              
            IF       RDF-KEYS EQUAL TO 900009000000000                   
                     PERFORM PASS-1 GO TO COPY-WRITE-8.                  
            GO       TO COPY-FAIL-1-8.                                   
        COPY-DELETE-8.                                                   
            PERFORM  DE-LETE-1.                                          
            GO       TO COPY-WRITE-8.                                    
        COPY-FAIL-1-8.                                                   
            MOVE     RDF-KEYS TO COMPUTED-18V0.                          
            MOVE     900009000000000 TO CORRECT-18V0.                    
            PERFORM  FAIL-1.                                             
        COPY-WRITE-8.                                                    
            MOVE     "COPY-TEST-8 " TO PAR-NAME.                         
            PERFORM  PRINT-DETAIL-1.                                     
        OUTP2 SECTION.                                                   
        COPY-TEST-9.                                                     
            RETURN   SORTFILE-2E END                                     
                     PERFORM PASS-1 GO TO COPY-WRITE-9.                  
            MOVE     RDF-KEYS TO COMPUTED-18V0.                          
            MOVE     "END OF FILE NOT FOUND" TO RE-MARK.                 
        COPY-DELETE-9.                                                   
            PERFORM  DE-LETE-1.                                          
        COPY-WRITE-9.                                                    
            MOVE     "COPY-TEST-9 " TO PAR-NAME.                         
            PERFORM  PRINT-DETAIL-1.                                     
            CLOSE    SORTOUT-2E.                                         
            GO       TO LIB2E-EXIT.                                      
        OUTP3 SECTION.                                                   
        RET-1.                                                           
            RETURN   SORTFILE-2E RECORD AT END GO TO BAD-FILE.           
            MOVE     S-RECORD TO SORTED.                                 
            WRITE    SORTED.                                             
        RET-2.                                                           
            RETURN   SORTFILE-2E           END GO TO BAD-FILE.           
            MOVE     S-RECORD TO SORTED.                                 
            WRITE    SORTED.                                             
        BAD-FILE.                                                        
            PERFORM  FAIL-1.                                             
            MOVE     "BAD-FILE" TO PAR-NAME.                             
            MOVE     "EOF PREMATURELY FOUND" TO RE-MARK.                 
            PERFORM  PRINT-DETAIL-1.                                     
            CLOSE    SORTOUT-2E.                                         
            GO TO LIB2E-EXIT.                                            
        INSPT-1. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.       
        PASS-1.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.         
        FAIL-1.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.        
        DE-LETE-1.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.    
            MOVE "****TEST DELETED****" TO RE-MARK.                      
        PRINT-DETAIL-1.                                                  
            IF REC-CT NOT EQUAL TO ZERO                                  
                    MOVE "." TO PARDOT-X                                 
                    MOVE REC-CT TO DOTVALUE.                             
            MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE-1.    
            IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE-1             
               PERFORM FAIL-ROUTINE-1 THRU FAIL-ROUTINE-EX-1             
                 ELSE PERFORM BAIL-OUT-1 THRU BAIL-OUT-EX-1.             
            MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              
            MOVE SPACE TO CORRECT-X.                                     
            IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         
            MOVE     SPACE TO RE-MARK.                                   
        WRITE-LINE-1.                                                    
            ADD 1 TO RECORD-COUNT.                                       
            IF RECORD-COUNT GREATER 50                                   
                MOVE DUMMY-RECORD TO DUMMY-HOLD                          
                MOVE SPACE TO DUMMY-RECORD                               
                WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  
                MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN-1           
                MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN-1 2 TIMES   
                MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN-1        
                MOVE DUMMY-HOLD TO DUMMY-RECORD                          
                MOVE ZERO TO RECORD-COUNT.                               
            PERFORM WRT-LN-1.                                            
        WRT-LN-1.                                                        
            WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               
            MOVE SPACE TO DUMMY-RECORD.                                  
        BLANK-LINE-PRINT-1.                                              
            PERFORM WRT-LN-1.                                            
        FAIL-ROUTINE-1.                                                  
            IF COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-RTN-WRITE-1.     
            IF CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-RTN-WRITE-1.      
            MOVE "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.    
            MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE-1 2 TIMES.   
            GO TO FAIL-ROUTINE-EX-1.                                     
        FAIL-RTN-WRITE-1.                                                
            MOVE TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE-1         
            MOVE TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE-1 2 TIMES. 
        FAIL-ROUTINE-EX-1. EXIT.                                         
        BAIL-OUT-1.                                                      
            IF COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE-1.     
            IF CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX-1.             
        BAIL-OUT-WRITE-1.                                                
            MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  
            MOVE XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE-1 2 TIMES.   
        BAIL-OUT-EX-1. EXIT.                                             
        LIB2E-EXIT.                                                      
            EXIT.                                                        
        """.trimIndent()

    val sm206A =
        """
        IDENTIFICATION DIVISION.                                         
        PROGRAM-ID.                                                      
            SM206A.                                                      
        ENVIRONMENT DIVISION.                                            
        CONFIGURATION SECTION.                                           
        SOURCE-COMPUTER.                                                 
            XXXXX082.                                                    
        OBJECT-COMPUTER.                                                 
            XXXXX083.                                                    
        INPUT-OUTPUT SECTION.                                            
        FILE-CONTROL.                                                    
            SELECT PRINT-FILE ASSIGN TO                                  
            XXXXX055.                                                    
        DATA DIVISION.                                                   
        FILE SECTION.                                                    
        FD  PRINT-FILE.                                                  
        01  PRINT-REC PICTURE X(120).                                    
        01  DUMMY-RECORD PICTURE X(120).                                 
        WORKING-STORAGE SECTION.                                         
        01  GRP-001.                                                     
            02 GRP-002.                                                  
               04 GRP-004.                                               
                  06 GRP-006.                                            
                     08 WRK-XN-00005-001 PIC X(5) VALUE "FIRST".         
                     08 WRK-XN-00050-O005F-001  OCCURS 5 TIMES.          
                        10 WRK-XN-00005-O005-001 PIC X(5).               
                        10 WRK-DS-05V00-O005-001 PIC S9(5).              
            02 GRP-003.                                                  
               04 GRP-004.                                               
                  06 GRP-006.                                            
                     08 WRK-XN-00005-001 PIC X(5) VALUE "SECON".         
                     08 WRK-XN-00050-O005F-001 OCCURS 5 TIMES.           
                        10 WRK-XN-00005-O005-001 PIC X(5).               
                        10 WRK-DS-05V00-O005-001 PIC S9(5).              
        01  GRP-007.                                                     
                     08 WRK-XN-00005-001 PIC X(5) VALUE "THIRD".         
        01  WRK-DS-09V00-901 PIC S9(9) VALUE ZERO.                       
        01  WRK-DS-09V00-902 PIC S9(9) VALUE ZERO.                       
        01  WRK-XN-00001     PIC  X.                                     
        01  WRK-XN-00322     PIC  X(322).                                
        01  FILLER REDEFINES WRK-XN-00322.                               
          03  WRK-XN-00322-1         PIC X.                              
          03  WRK-XN-00322-2-322.                                        
            05  WRK-XN-00322-2-3     PIC X.                              
            05  WRK-XN-00322-20      PIC X(20)                           
                                     OCCURS 16                           
                                     INDEXED BY X1.                      
        01  WRK-DU-9                    PIC 9          VALUE ZERO.       
        01  WRK-DU-99                   PIC 99         VALUE ZERO.       
        01  WRK-DU-99-LONGER            PIC 99         VALUE ZERO.       
        01  TEST-RESULTS.                                                
            02 FILLER                   PIC X      VALUE SPACE.          
            02 FEATURE                  PIC X(20)  VALUE SPACE.          
            02 FILLER                   PIC X      VALUE SPACE.          
            02 P-OR-F                   PIC X(5)   VALUE SPACE.          
            02 FILLER                   PIC X      VALUE SPACE.          
            02  PAR-NAME.                                                
              03 FILLER                 PIC X(19)  VALUE SPACE.          
              03  PARDOT-X              PIC X      VALUE SPACE.          
              03 DOTVALUE               PIC 99     VALUE ZERO.           
            02 FILLER                   PIC X(8)   VALUE SPACE.          
            02 RE-MARK                  PIC X(61).                       
        01  TEST-COMPUTED.                                               
            02 FILLER                   PIC X(30)  VALUE SPACE.          
            02 FILLER                   PIC X(17)  VALUE                 
                   "       COMPUTED=".                                   
            02 COMPUTED-X.                                               
            03 COMPUTED-A               PIC X(20)  VALUE SPACE.          
            03 COMPUTED-N               REDEFINES COMPUTED-A             
                                        PIC -9(9).9(9).                  
            03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         
            03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     
            03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     
            03       CM-18V0 REDEFINES COMPUTED-A.                       
                04 COMPUTED-18V0                    PIC -9(18).          
                04 FILLER                           PIC X.               
            03 FILLER PIC X(50) VALUE SPACE.                             
        01  TEST-CORRECT.                                                
            02 FILLER PIC X(30) VALUE SPACE.                             
            02 FILLER PIC X(17) VALUE "       CORRECT =".                
            02 CORRECT-X.                                                
            03 CORRECT-A                  PIC X(20) VALUE SPACE.         
            03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      
            03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         
            03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     
            03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     
            03      CR-18V0 REDEFINES CORRECT-A.                         
                04 CORRECT-18V0                     PIC -9(18).          
                04 FILLER                           PIC X.               
            03 FILLER PIC X(2) VALUE SPACE.                              
            03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     
        01  CCVS-C-1.                                                    
            02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PASS  PARAGRAPH-NAME                                                 REMARKS".                                            
            02 FILLER                     PIC X(20)    VALUE SPACE.      
        01  CCVS-C-2.                                                    
            02 FILLER                     PIC X        VALUE SPACE.      
            02 FILLER                     PIC X(6)     VALUE "TESTED".   
            02 FILLER                     PIC X(15)    VALUE SPACE.      
            02 FILLER                     PIC X(4)     VALUE "FAIL".     
            02 FILLER                     PIC X(94)    VALUE SPACE.      
        01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       
        01  REC-CT                        PIC 99       VALUE ZERO.       
        01  DELETE-COUNTER                PIC 999      VALUE ZERO.       
        01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       
        01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       
        01  PASS-COUNTER                  PIC 999      VALUE ZERO.       
        01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       
        01  ERROR-HOLD                    PIC 999      VALUE ZERO.       
        01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      
        01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       
        01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     
        01  CCVS-H-1.                                                    
            02  FILLER                    PIC X(39)    VALUE SPACES.     
            02  FILLER                    PIC X(42)    VALUE             
            "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 
            02  FILLER                    PIC X(39)    VALUE SPACES.     
        01  CCVS-H-2A.                                                   
          02  FILLER                        PIC X(40)  VALUE SPACE.      
          02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  
          02  FILLER                        PIC XXXX   VALUE             
            "4.2 ".                                                      
          02  FILLER                        PIC X(28)  VALUE             
                   " COPY - NOT FOR DISTRIBUTION".                       
          02  FILLER                        PIC X(41)  VALUE SPACE.      
        01  CCVS-H-2B.                                                   
          02  FILLER                        PIC X(15)  VALUE             
                   "TEST RESULT OF ".                                    
          02  TEST-ID                       PIC X(9).                    
          02  FILLER                        PIC X(4)   VALUE             
                   " IN ".                                               
          02  FILLER                        PIC X(12)  VALUE             
            " HIGH       ".                                              
          02  FILLER                        PIC X(22)  VALUE             
                   " LEVEL VALIDATION FOR ".                             
          02  FILLER                        PIC X(58)  VALUE             
            "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".
        01  CCVS-H-3.                                                    
            02  FILLER                      PIC X(34)  VALUE             
                   " FOR OFFICIAL USE ONLY    ".                         
            02  FILLER                      PIC X(58)  VALUE             
            "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".
            02  FILLER                      PIC X(28)  VALUE             
                   "  COPYRIGHT   1985 ".                                
        01  CCVS-E-1.                                                    
            02 FILLER                       PIC X(52)  VALUE SPACE.      
            02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              
            02 ID-AGAIN                     PIC X(9).                    
            02 FILLER                       PIC X(45)  VALUE SPACES.     
        01  CCVS-E-2.                                                    
            02  FILLER                      PIC X(31)  VALUE SPACE.      
            02  FILLER                      PIC X(21)  VALUE SPACE.      
            02 CCVS-E-2-2.                                               
                03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      
                03 FILLER                   PIC X      VALUE SPACE.      
                03 ENDER-DESC               PIC X(44)  VALUE             
                   "ERRORS ENCOUNTERED".                                 
        01  CCVS-E-3.                                                    
            02  FILLER                      PIC X(22)  VALUE             
                   " FOR OFFICIAL USE ONLY".                             
            02  FILLER                      PIC X(12)  VALUE SPACE.      
            02  FILLER                      PIC X(58)  VALUE             
            "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".
            02  FILLER                      PIC X(13)  VALUE SPACE.      
            02 FILLER                       PIC X(15)  VALUE             
                    " COPYRIGHT 1985".                                   
        01  CCVS-E-4.                                                    
            02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      
            02 FILLER                       PIC X(4)   VALUE " OF ".     
            02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      
            02 FILLER                       PIC X(40)  VALUE             
             "  TESTS WERE EXECUTED SUCCESSFULLY".                       
        01  XXINFO.                                                      
            02 FILLER                       PIC X(19)  VALUE             
                   "*** INFORMATION ***".                                
            02 INFO-TEXT.                                                
              04 FILLER                     PIC X(8)   VALUE SPACE.      
              04 XXCOMPUTED                 PIC X(20).                   
              04 FILLER                     PIC X(5)   VALUE SPACE.      
              04 XXCORRECT                  PIC X(20).                   
            02 INF-ANSI-REFERENCE           PIC X(48).                   
        01  HYPHEN-LINE.                                                 
            02 FILLER  PIC IS X VALUE IS SPACE.                          
            02 FILLER  PIC IS X(65)    VALUE IS "*****************************************************************".                 
            02 FILLER  PIC IS X(54)    VALUE IS "******************************************************".                            
        01  CCVS-PGM-ID                     PIC X(9)   VALUE             
            "SM206A".                                                    
        PROCEDURE DIVISION.                                              
        CCVS1 SECTION.                                                   
        OPEN-FILES.                                                      
            OPEN     OUTPUT PRINT-FILE.                                  
            MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   
            MOVE    SPACE TO TEST-RESULTS.                               
            PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             
            GO TO CCVS1-EXIT.                                            
        CLOSE-FILES.                                                     
            PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   
        TERMINATE-CCVS.                                                  
            EXIT PROGRAM.                                                
        TERMINATE-CALL.                                                  
            STOP     RUN.                                                
        INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         
        PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           
        FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          
        DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      
            MOVE "****TEST DELETED****" TO RE-MARK.                      
        PRINT-DETAIL.                                                    
            IF REC-CT NOT EQUAL TO ZERO                                  
                    MOVE "." TO PARDOT-X                                 
                    MOVE REC-CT TO DOTVALUE.                             
            MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      
            IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               
               PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 
                 ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 
            MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              
            MOVE SPACE TO CORRECT-X.                                     
            IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         
            MOVE     SPACE TO RE-MARK.                                   
        HEAD-ROUTINE.                                                    
            MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  
            MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  
            MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  
            MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  
        COLUMN-NAMES-ROUTINE.                                            
            MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           
            MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   
            MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        
        END-ROUTINE.                                                     
            MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.
        END-RTN-EXIT.                                                    
            MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   
        END-ROUTINE-1.                                                   
             ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      
             ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               
             ADD PASS-COUNTER TO ERROR-HOLD.                             
             MOVE PASS-COUNTER TO CCVS-E-4-1.                            
             MOVE ERROR-HOLD TO CCVS-E-4-2.                              
             MOVE CCVS-E-4 TO CCVS-E-2-2.                                
             MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           
         END-ROUTINE-12.                                                 
             MOVE "TEST(S) FAILED" TO ENDER-DESC.                        
            IF       ERROR-COUNTER IS EQUAL TO ZERO                      
                MOVE "NO " TO ERROR-TOTAL                                
                ELSE                                                     
                MOVE ERROR-COUNTER TO ERROR-TOTAL.                       
            MOVE     CCVS-E-2 TO DUMMY-RECORD.                           
            PERFORM WRITE-LINE.                                          
        END-ROUTINE-13.                                                  
            IF DELETE-COUNTER IS EQUAL TO ZERO                           
                MOVE "NO " TO ERROR-TOTAL  ELSE                          
                MOVE DELETE-COUNTER TO ERROR-TOTAL.                      
            MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   
            MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           
             IF   INSPECT-COUNTER EQUAL TO ZERO                          
                 MOVE "NO " TO ERROR-TOTAL                               
             ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   
             MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            
             MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          
            MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           
        WRITE-LINE.                                                      
            ADD 1 TO RECORD-COUNT.                                       
            IF RECORD-COUNT GREATER 50                                   
                MOVE DUMMY-RECORD TO DUMMY-HOLD                          
                MOVE SPACE TO DUMMY-RECORD                               
                WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  
                MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             
                MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     
                MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          
                MOVE DUMMY-HOLD TO DUMMY-RECORD                          
                MOVE ZERO TO RECORD-COUNT.                               
            PERFORM WRT-LN.                                              
        WRT-LN.                                                          
            WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               
            MOVE SPACE TO DUMMY-RECORD.                                  
        BLANK-LINE-PRINT.                                                
            PERFORM WRT-LN.                                              
        FAIL-ROUTINE.                                                    
            IF   COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE. 
            IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.
            MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 
            MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   
            MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   
            MOVE   SPACES TO INF-ANSI-REFERENCE.                         
            GO TO  FAIL-ROUTINE-EX.                                      
        FAIL-ROUTINE-WRITE.                                              
            MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         
            MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 
            MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. 
            MOVE   SPACES TO COR-ANSI-REFERENCE.                         
        FAIL-ROUTINE-EX. EXIT.                                           
        BAIL-OUT.                                                        
            IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   
            IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           
        BAIL-OUT-WRITE.                                                  
            MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  
            MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 
            MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   
            MOVE   SPACES TO INF-ANSI-REFERENCE.                         
        BAIL-OUT-EX. EXIT.                                               
        CCVS1-EXIT.                                                      
            EXIT.                                                        
        SECT-SM206-0001 SECTION.                                         
        PST-TEST-001.                                                    
            MOVE    "PSEUDO-TEXT" TO FEATURE.                            
            MOVE    "PST-TEST-001" TO PAR-NAME                           
            PERFORM PASS.                                                
        PST-WRITE-001.                                                   
            PERFORM PRINT-DETAIL.                                        
        SECT-SM206-0002 SECTION.                                         
        PST-INIT-002.                                                    
            MOVE   +00005 TO WRK-DS-05V00-O005-001 OF GRP-002 (1).       
            MOVE   +000000005 TO WRK-DS-09V00-901.                       
        PST-TEST-002.                                                    
            MOVE    "PSEUDO-TEXT/IDENTIFR" TO FEATURE.                   
            MOVE   +00009 TO WRK-DS-05V00-O005-001  IN WRK-XN-00050-O005F-001 OF GRP-006 OF GRP-004 IN GRP-003 ( 2 ).         
            ADD                                                          
                +00001 TO                                                
                     WRK-DS-05V00-O005-001                               
                                                IN                       
                              WRK-XN-00050-O005F-001                     
                                                      IN                 
                                       GRP-006 IN                                          GRP-004                                                 IN                                              GRP-002                                                     IN          
         GRP-001 (1)
        .         
            MOVE    "PST-TEST-002" TO PAR-NAME.                          
            MOVE    01 TO REC-CT.                                        
            IF      WRK-DS-05V00-O005-001 OF GRP-002 (1) EQUAL TO +6     
                    PERFORM PASS                                         
                    ELSE                                                 
                    MOVE +6 TO CORRECT-18V0                              
                    MOVE WRK-DS-05V00-O005-001 OF GRP-002 (1) TO         
                        COMPUTED-18V0                                    
                    PERFORM FAIL.                                        
            PERFORM PRINT-DETAIL.                                        
            ADD     +01 TO REC-CT.                                       
            IF      WRK-DS-09V00-901 NOT EQUAL TO +5                     
                    MOVE +5 TO CORRECT-18V0                              
                    MOVE WRK-DS-09V00-901 TO COMPUTED-18V0               
                    PERFORM FAIL                                         
                    ELSE                                                 
                    PERFORM PASS.                                        
            PERFORM PRINT-DETAIL.                                        
            ADD     +01 TO REC-CT.                                       
            IF      WRK-DS-05V00-O005-001 IN WRK-XN-00050-O005F-001 IN   
                    GRP-006 IN GRP-004 IN GRP-003 (2) EQUAL TO +9        
                    PERFORM PASS                                         
                    ELSE                                                 
                    MOVE WRK-DS-05V00-O005-001   IN                      
                         WRK-XN-00050-O005F-001  IN                      
                         GRP-006                 IN                      
                         GRP-004                 IN                      
                         GRP-003 (2) TO COMPUTED-18V0                    
                    MOVE +9 TO  CORRECT-18V0                             
                    PERFORM FAIL.                                        
            PERFORM PRINT-DETAIL.                                        
        PST-INIT-003.                                                    
            MOVE "PSEUDO-TEXT/LITERAL" TO FEATURE.                       
            MOVE "PST-TEST-003" TO PAR-NAME.                             
            MOVE    +00005 TO WRK-DS-05V00-O005-001 OF GRP-002 (3).      
            MOVE    +000000005 TO WRK-DS-09V00-901.                      
            MOVE    ZERO TO  WRK-DS-05V00-O005-001 IN GRP-003 (3).       
        PST-TEST-003.                                                    
            MOVE    +0009 TO WRK-DS-05V00-O005-001  IN GRP-003 (3).      
            ADD         +2 TO WRK-DS-09V00-901.                          
            SUBTRACT            -3 FROM WRK-DS-05V00-O005-001 IN GRP-002 (3).        
        PST-EXIT-003-X.                                                  
        PST-TEST-003-1.                                                  
            MOVE    01 TO REC-CT.                                        
            IF      WRK-DS-05V00-O005-001 IN GRP-003 (3) EQUAL TO +00009 
                    PERFORM PASS                                         
                    ELSE                                                 
                    MOVE   +009 TO CORRECT-18V0                          
                    MOVE   WRK-DS-05V00-O005-001  IN                     
                           GRP-003 (3) TO COMPUTED-18V0                  
                    PERFORM FAIL.                                        
            PERFORM PRINT-DETAIL.                                        
            ADD     +01 TO REC-CT.                                       
            IF      WRK-DS-09V00-901 EQUAL TO +000000007                 
                    PERFORM PASS                                         
                    ELSE                                                 
                    PERFORM FAIL                                         
                    MOVE  +7 TO CORRECT-18V0                             
                    MOVE  WRK-DS-09V00-901 TO COMPUTED-18V0.             
            PERFORM PRINT-DETAIL.                                        
            ADD     +01 TO REC-CT.                                       
            IF      WRK-DS-05V00-O005-001 OF GRP-002 (3) EQUAL TO +8     
                    PERFORM PASS                                         
                    ELSE                                                 
                    MOVE +8 TO CORRECT-18V0                              
                    PERFORM FAIL                                         
                    MOVE WRK-DS-05V00-O005-001 IN GRP-002 (3) TO         
                    COMPUTED-18V0.                                       
            PERFORM PRINT-DETAIL.                                        
            MOVE 0 TO WRK-DS-09V00-901.                                  
        PST-INIT-004.                                                    
            MOVE "PSEUDO-TEXT/WORD" TO FEATURE.                          
            MOVE    ZERO TO WRK-DS-09V00-901.                            
            MOVE    "PST-TEST-004" TO PAR-NAME.                          
        PST-TEST-004.                                                    
            ADD     5 TO WRK-DS-09V00-901.                               
            MOVE                                                         
         +2 TO WRK-DS-09V00-902.                        
            GO TO   PST-EXIT-004.                                        
        PST-DELETE-004.                                                  
            PERFORM           DE-LETE.                                              
        PST-EXIT-004.                                                    
            EXIT.                                                        
        PST-WRITE-004.                                                   
            MOVE    "PST-TEST-004" TO PAR-NAME.                          
            MOVE    01 TO REC-CT.                                        
            IF      WRK-DS-09V00-901 EQUAL TO 5                          
                    PERFORM PASS                                         
                    ELSE                                                 
                    PERFORM FAIL                                         
                    MOVE 5 TO CORRECT-18V0                               
                    MOVE WRK-DS-09V00-901 TO COMPUTED-18V0.              
            PERFORM PRINT-DETAIL.                                        
            ADD     1 TO REC-CT.                                         
            IF      WRK-DS-09V00-902 EQUAL TO 2                          
                    PERFORM PASS                                         
                    ELSE                                                 
                    MOVE 2 TO CORRECT-18V0                               
                    MOVE WRK-DS-09V00-902 TO COMPUTED-18V0               
                    PERFORM FAIL.                                        
            PERFORM PRINT-DETAIL.                                        
        PST-TEST-005.                                                    
            MOVE 0 TO WRK-DS-09V00-901.                                  
            MOVE 7 TO WRK-DS-09V00-901.                                  
            IF WRK-DS-09V00-901 IS EQUAL TO 5                            
                PERFORM PASS   GO TO PST-WRITE-005.                      
            PERFORM FAIL.                                                
            MOVE WRK-DS-09V00-901 TO COMPUTED-18V0.                      
            MOVE 5 TO CORRECT-18V0.                                      
            IF WRK-DS-09V00-901 IS EQUAL TO 7                            
                MOVE "CASCADED REPLACEMENT PERFORMED" TO RE-MARK.        
            GO TO PST-WRITE-005.                                         
        PST-DELETE-005.                                                  
            PERFORM DE-LETE.                                             
        PST-WRITE-005.                                                   
            MOVE "CASCADED REPLACE PST" TO FEATURE.                      
            MOVE "PST-TEST-005" TO PAR-NAME.                             
            MOVE 01 TO REC-CT.                                           
            PERFORM PRINT-DETAIL.                                        
        PST-TEST-006.                                                    
            MOVE 0 TO WRK-DS-09V00-901.                                  
            ADD      001005 TO WRK-DS-09V00-901.                            
            IF WRK-DS-09V00-901 IS EQUAL TO 1005                         
                PERFORM PASS   GO TO PST-WRITE-006.                      
            PERFORM FAIL.                                                
            MOVE WRK-DS-09V00-901 TO COMPUTED-18V0.                      
            MOVE 1005 TO CORRECT-18V0.                                   
            IF WRK-DS-09V00-901 IS EQUAL TO 10                           
                MOVE "PART REPLACING, CONT IGNORED" TO RE-MARK.          
            IF WRK-DS-09V00-901 IS EQUAL TO 37                           
                MOVE "PART REPLACING, CONT HONORED" TO RE-MARK.          
            GO TO PST-WRITE-006.                                         
        PST-DELETE-006.                                                  
            PERFORM DE-LETE.                                             
        PST-WRITE-006.                                                   
            MOVE "CONT LIT/PST PART RPL" TO FEATURE.                     
            MOVE "PST-TEST-006" TO PAR-NAME.                             
            PERFORM PRINT-DETAIL.                                        
        PST-TEST-007.                                                    
            PERFORM FAIL.                                                
            SUBTRACT 1 FROM ERROR-COUNTER.                               
            PERFORM PASS.                                                
            IF P-OR-F IS EQUAL TO "FAIL*"  ADD 1 TO ERROR-COUNTER.       
            GO TO PST-WRITE-007.                                         
        PST-DELETE-007.                                                  
            PERFORM DE-LETE.                                             
        PST-WRITE-007.                                                   
            MOVE "PST/EMBEDDED COMMENT" TO FEATURE.                      
            MOVE "PST-TEST-007" TO PAR-NAME.                             
            MOVE 01 TO REC-CT.                                           
            PERFORM PRINT-DETAIL.                                        
        PST-TEST-008.                                                    
        PST-DELETE-008.                                                  
            PERFORM DE-LETE.                                             
        PST-WRITE-008.                                                   
            MOVE "COPY IN DEBUG LINE" TO FEATURE.                        
            MOVE "PST-TEST-008" TO PAR-NAME.                             
            PERFORM PRINT-DETAIL.                                        
        PST-TEST-009.                                                    
            PERFORM FAIL.                                                
            SUBTRACT 1 FROM ERROR-COUNTER.                               
            PERFORM PASS.                                                
            IF P-OR-F IS EQUAL TO "FAIL*"  ADD 1 TO ERROR-COUNTER.       
            GO TO PST-WRITE-009.                                         
        PST-DELETE-009.                                                  
            PERFORM DE-LETE.                                             
        PST-WRITE-009.                                                   
            MOVE "DEBUG LINE IN TEXT" TO FEATURE.                        
            MOVE "PST-TEST-009" TO PAR-NAME.                             
            PERFORM PRINT-DETAIL.                                        
        PST-TEST-10.                                                     
            MOVE   "XII-2 2.3 SR8" TO ANSI-REFERENCE.                    
            MOVE   "PST-TEST-10"   TO PAR-NAME.                          
            MOVE   "T" TO WRK-XN-00001.                                  
            GO TO   PST-TEST-10-0.                                       
        PST-DELETE-10.                                                   
            PERFORM DE-LETE.                                             
            PERFORM PRINT-DETAIL.                                        
            GO TO   PST-INIT-11.                                         
        PST-TEST-10-0.                                                   
            IF      WRK-XN-00001 = "T"                                   
                    PERFORM PASS                                         
                    PERFORM PRINT-DETAIL                                 
            ELSE                                                         
                    MOVE   "REPLACING SINGLE CHARACTER FAILED"           
                         TO RE-MARK                                      
                    MOVE   "T"  TO CORRECT-X                             
                    MOVE    WRK-XN-00001 TO COMPUTED-X                   
                    PERFORM FAIL                                         
                    PERFORM PRINT-DETAIL.                                
        PST-INIT-11.                                                     
            MOVE   "XII-2 2.3 (SR8) AND XII-5 2.4(GR11)"                 
                 TO ANSI-REFERENCE.                                      
            MOVE   "PST-TEST-11" TO PAR-NAME.                            
            MOVE    SPACES      TO WRK-XN-00322.                         
            MOVE    1 TO REC-CT.                                         
        REP-TEST-11-0.                                                   
        PST-DELETE-11.                                                   
            PERFORM DE-LETE.                                             
            PERFORM PRINT-DETAIL.                                        
            GO TO   CCVS-EXIT.                                           
        PST-TEST-11-1.                                                   
            MOVE   "PST-TEST-11-1" TO PAR-NAME.                          
            IF      WRK-DU-9 = 6                                         
                    PERFORM PASS                                         
                    PERFORM PRINT-DETAIL                                 
            ELSE                                                         
                    MOVE   "COPYING ALL 322 CHARACTERS FAILED"           
                         TO RE-MARK                                      
                    MOVE    6   TO CORRECT-N                             
                    MOVE    WRK-DU-9 TO COMPUTED-N                       
                    PERFORM FAIL                                         
                    PERFORM PRINT-DETAIL.                                
            ADD     1 TO REC-CT.                                         
        PST-TEST-11-2.                                                   
            MOVE   "PST-TEST-11-2" TO PAR-NAME.                          
            IF      WRK-DU-99 = 9                                        
                    PERFORM PASS                                         
                    PERFORM PRINT-DETAIL                                 
            ELSE                                                         
                    MOVE   "COPYING ALL 322 CHARACTERS FAILED"           
                         TO RE-MARK                                      
                    MOVE    9   TO CORRECT-N                             
                    MOVE    WRK-DU-99 TO COMPUTED-N                      
                    PERFORM FAIL                                         
                    PERFORM PRINT-DETAIL.                                
            ADD     1 TO REC-CT.                                         
        PST-TEST-11-3.                                                   
            MOVE   "PST-TEST-11-3" TO PAR-NAME.                          
            IF      WRK-DU-99-LONGER = 10                                
                    PERFORM PASS                                         
                    PERFORM PRINT-DETAIL                                 
            ELSE                                                         
                    MOVE   "COPYING ALL 322 CHARACTERS FAILED"           
                         TO RE-MARK                                      
                    MOVE    10  TO CORRECT-N                             
                    MOVE    WRK-DU-99-LONGER TO COMPUTED-N               
                    PERFORM FAIL                                         
                    PERFORM PRINT-DETAIL.                                
        CCVS-EXIT SECTION.                                               
        CCVS-999999.                                                     
            GO TO CLOSE-FILES.                                           
        """.trimIndent()

    val sm208A =
        """
        IDENTIFICATION DIVISION.                                         
        PROGRAM-ID. SM208A.                                              
        ENVIRONMENT DIVISION.                                            
        CONFIGURATION SECTION.                                           
        SOURCE-COMPUTER.                                                 
            XXXXX082.                                                    
        OBJECT-COMPUTER.                                                 
            XXXXX083.                                                    
        INPUT-OUTPUT SECTION.                                            
        FILE-CONTROL.                                                    
            SELECT PRINT-FILE ASSIGN TO                                  
            XXXXX055.                                                    
        DATA DIVISION.                                                   
        FILE SECTION.                                                    
        FD  PRINT-FILE.                                                  
        01  PRINT-REC PICTURE X(120).                                    
        01  DUMMY-RECORD PICTURE X(120).                                 
        WORKING-STORAGE SECTION.                                         
        01  A         PIC X.                                             
        01  B         PIC S9(7) COMP.                                    
        01  C         PIC XXBXX/XX.                                      
        01  D     PICTURE X(7) VALUE "PICTURE".                          
        01  WRK-XN-00001  PIC X.                                         
        01  WRK-XN-00020  PIC X(20).                                     
        01  WRK-XN-00322  PIC X(322).                                    
        01  FILLER REDEFINES WRK-XN-00322.                               
          03  WRK-XN-00322-1         PIC X.                              
          03  WRK-XN-00322-2-322.                                        
            05  WRK-XN-00322-2       PIC X.                              
            05  WRK-XN-00322-20      PIC X(20)                           
                                     OCCURS 16                           
                                     INDEXED BY X1.                      
        01  WS-A          PIC X.                                         
        01  WS-B          PIC X.                                         
        01  WS-C          PIC X.                                         
        01  WS-D          PIC X.                                         
        01  WS-E          PIC X.                                         
        01  WS-F          PIC X.                                         
        01  TEST-RESULTS.                                                
            02 FILLER                   PIC X      VALUE SPACE.          
            02 FEATURE                  PIC X(20)  VALUE SPACE.          
            02 FILLER                   PIC X      VALUE SPACE.          
            02 P-OR-F                   PIC X(5)   VALUE SPACE.          
            02 FILLER                   PIC X      VALUE SPACE.          
            02  PAR-NAME.                                                
              03 FILLER                 PIC X(19)  VALUE SPACE.          
              03  PARDOT-X              PIC X      VALUE SPACE.          
              03 DOTVALUE               PIC 99     VALUE ZERO.           
            02 FILLER                   PIC X(8)   VALUE SPACE.          
            02 RE-MARK                  PIC X(61).                       
        01  TEST-COMPUTED.                                               
            02 FILLER                   PIC X(30)  VALUE SPACE.          
            02 FILLER                   PIC X(17)  VALUE                 
                   "       COMPUTED=".                                   
            02 COMPUTED-X.                                               
            03 COMPUTED-A               PIC X(20)  VALUE SPACE.          
            03 COMPUTED-N               REDEFINES COMPUTED-A             
                                        PIC -9(9).9(9).                  
            03 COMPUTED-0V18 REDEFINES COMPUTED-A   PIC -.9(18).         
            03 COMPUTED-4V14 REDEFINES COMPUTED-A   PIC -9(4).9(14).     
            03 COMPUTED-14V4 REDEFINES COMPUTED-A   PIC -9(14).9(4).     
            03       CM-18V0 REDEFINES COMPUTED-A.                       
                04 COMPUTED-18V0                    PIC -9(18).          
                04 FILLER                           PIC X.               
            03 FILLER PIC X(50) VALUE SPACE.                             
        01  TEST-CORRECT.                                                
            02 FILLER PIC X(30) VALUE SPACE.                             
            02 FILLER PIC X(17) VALUE "       CORRECT =".                
            02 CORRECT-X.                                                
            03 CORRECT-A                  PIC X(20) VALUE SPACE.         
            03 CORRECT-N    REDEFINES CORRECT-A     PIC -9(9).9(9).      
            03 CORRECT-0V18 REDEFINES CORRECT-A     PIC -.9(18).         
            03 CORRECT-4V14 REDEFINES CORRECT-A     PIC -9(4).9(14).     
            03 CORRECT-14V4 REDEFINES CORRECT-A     PIC -9(14).9(4).     
            03      CR-18V0 REDEFINES CORRECT-A.                         
                04 CORRECT-18V0                     PIC -9(18).          
                04 FILLER                           PIC X.               
            03 FILLER PIC X(2) VALUE SPACE.                              
            03 COR-ANSI-REFERENCE             PIC X(48) VALUE SPACE.     
        01  CCVS-C-1.                                                    
            02 FILLER  PIC IS X(99)    VALUE IS " FEATURE              PASS  PARAGRAPH-NAME                                                 REMARKS".                                            
            02 FILLER                     PIC X(20)    VALUE SPACE.      
        01  CCVS-C-2.                                                    
            02 FILLER                     PIC X        VALUE SPACE.      
            02 FILLER                     PIC X(6)     VALUE "TESTED".   
            02 FILLER                     PIC X(15)    VALUE SPACE.      
            02 FILLER                     PIC X(4)     VALUE "FAIL".     
            02 FILLER                     PIC X(94)    VALUE SPACE.      
        01  REC-SKL-SUB                   PIC 9(2)     VALUE ZERO.       
        01  REC-CT                        PIC 99       VALUE ZERO.       
        01  DELETE-COUNTER                PIC 999      VALUE ZERO.       
        01  ERROR-COUNTER                 PIC 999      VALUE ZERO.       
        01  INSPECT-COUNTER               PIC 999      VALUE ZERO.       
        01  PASS-COUNTER                  PIC 999      VALUE ZERO.       
        01  TOTAL-ERROR                   PIC 999      VALUE ZERO.       
        01  ERROR-HOLD                    PIC 999      VALUE ZERO.       
        01  DUMMY-HOLD                    PIC X(120)   VALUE SPACE.      
        01  RECORD-COUNT                  PIC 9(5)     VALUE ZERO.       
        01  ANSI-REFERENCE                PIC X(48)    VALUE SPACES.     
        01  CCVS-H-1.                                                    
            02  FILLER                    PIC X(39)    VALUE SPACES.     
            02  FILLER                    PIC X(42)    VALUE             
            "OFFICIAL COBOL COMPILER VALIDATION SYSTEM".                 
            02  FILLER                    PIC X(39)    VALUE SPACES.     
        01  CCVS-H-2A.                                                   
          02  FILLER                        PIC X(40)  VALUE SPACE.      
          02  FILLER                        PIC X(7)   VALUE "CCVS85 ".  
          02  FILLER                        PIC XXXX   VALUE             
            "4.2 ".                                                      
          02  FILLER                        PIC X(28)  VALUE             
                   " COPY - NOT FOR DISTRIBUTION".                       
          02  FILLER                        PIC X(41)  VALUE SPACE.      
        01  CCVS-H-2B.                                                   
          02  FILLER                        PIC X(15)  VALUE             
                   "TEST RESULT OF ".                                    
          02  TEST-ID                       PIC X(9).                    
          02  FILLER                        PIC X(4)   VALUE             
                   " IN ".                                               
          02  FILLER                        PIC X(12)  VALUE             
            " HIGH       ".                                              
          02  FILLER                        PIC X(22)  VALUE             
                   " LEVEL VALIDATION FOR ".                             
          02  FILLER                        PIC X(58)  VALUE             
            "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".
        01  CCVS-H-3.                                                    
            02  FILLER                      PIC X(34)  VALUE             
                   " FOR OFFICIAL USE ONLY    ".                         
            02  FILLER                      PIC X(58)  VALUE             
            "COBOL 85 VERSION 4.2, Apr  1993 SSVG                      ".
            02  FILLER                      PIC X(28)  VALUE             
                   "  COPYRIGHT   1985 ".                                
        01  CCVS-E-1.                                                    
            02 FILLER                       PIC X(52)  VALUE SPACE.      
            02 FILLER  PIC X(14) VALUE IS "END OF TEST-  ".              
            02 ID-AGAIN                     PIC X(9).                    
            02 FILLER                       PIC X(45)  VALUE SPACES.     
        01  CCVS-E-2.                                                    
            02  FILLER                      PIC X(31)  VALUE SPACE.      
            02  FILLER                      PIC X(21)  VALUE SPACE.      
            02 CCVS-E-2-2.                                               
                03 ERROR-TOTAL              PIC XXX    VALUE SPACE.      
                03 FILLER                   PIC X      VALUE SPACE.      
                03 ENDER-DESC               PIC X(44)  VALUE             
                   "ERRORS ENCOUNTERED".                                 
        01  CCVS-E-3.                                                    
            02  FILLER                      PIC X(22)  VALUE             
                   " FOR OFFICIAL USE ONLY".                             
            02  FILLER                      PIC X(12)  VALUE SPACE.      
            02  FILLER                      PIC X(58)  VALUE             
            "ON-SITE VALIDATION, NATIONAL INSTITUTE OF STD & TECH.     ".
            02  FILLER                      PIC X(13)  VALUE SPACE.      
            02 FILLER                       PIC X(15)  VALUE             
                    " COPYRIGHT 1985".                                   
        01  CCVS-E-4.                                                    
            02 CCVS-E-4-1                   PIC XXX    VALUE SPACE.      
            02 FILLER                       PIC X(4)   VALUE " OF ".     
            02 CCVS-E-4-2                   PIC XXX    VALUE SPACE.      
            02 FILLER                       PIC X(40)  VALUE             
             "  TESTS WERE EXECUTED SUCCESSFULLY".                       
        01  XXINFO.                                                      
            02 FILLER                       PIC X(19)  VALUE             
                   "*** INFORMATION ***".                                
            02 INFO-TEXT.                                                
              04 FILLER                     PIC X(8)   VALUE SPACE.      
              04 XXCOMPUTED                 PIC X(20).                   
              04 FILLER                     PIC X(5)   VALUE SPACE.      
              04 XXCORRECT                  PIC X(20).                   
            02 INF-ANSI-REFERENCE           PIC X(48).                   
        01  HYPHEN-LINE.                                                 
            02 FILLER  PIC IS X VALUE IS SPACE.                          
            02 FILLER  PIC IS X(65)    VALUE IS "*****************************************************************".                 
            02 FILLER  PIC IS X(54)    VALUE IS "******************************************************".                            
        01  CCVS-PGM-ID                     PIC X(9)   VALUE             
            "SM208A".                                                    
        PROCEDURE DIVISION.                                              
        CCVS1 SECTION.                                                   
        OPEN-FILES.                                                      
            OPEN     OUTPUT PRINT-FILE.                                  
            MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   
            MOVE    SPACE TO TEST-RESULTS.                               
            PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             
            GO TO CCVS1-EXIT.                                            
        CLOSE-FILES.                                                     
            PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   
        TERMINATE-CCVS.                                                  
            EXIT PROGRAM.                                                
        TERMINATE-CALL.                                                  
            STOP     RUN.                                                
        INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         
        PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           
        FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          
        DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      
            MOVE "****TEST DELETED****" TO RE-MARK.                      
        PRINT-DETAIL.                                                    
            IF REC-CT NOT EQUAL TO ZERO                                  
                    MOVE "." TO PARDOT-X                                 
                    MOVE REC-CT TO DOTVALUE.                             
            MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      
            IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               
               PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 
                 ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 
            MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              
            MOVE SPACE TO CORRECT-X.                                     
            IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         
            MOVE     SPACE TO RE-MARK.                                   
        HEAD-ROUTINE.                                                    
            MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  
            MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  
            MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  
            MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  
        COLUMN-NAMES-ROUTINE.                                            
            MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           
            MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   
            MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        
        END-ROUTINE.                                                     
            MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.
        END-RTN-EXIT.                                                    
            MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   
        END-ROUTINE-1.                                                   
             ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      
             ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               
             ADD PASS-COUNTER TO ERROR-HOLD.                             
             MOVE PASS-COUNTER TO CCVS-E-4-1.                            
             MOVE ERROR-HOLD TO CCVS-E-4-2.                              
             MOVE CCVS-E-4 TO CCVS-E-2-2.                                
             MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           
         END-ROUTINE-12.                                                 
             MOVE "TEST(S) FAILED" TO ENDER-DESC.                        
            IF       ERROR-COUNTER IS EQUAL TO ZERO                      
                MOVE "NO " TO ERROR-TOTAL                                
                ELSE                                                     
                MOVE ERROR-COUNTER TO ERROR-TOTAL.                       
            MOVE     CCVS-E-2 TO DUMMY-RECORD.                           
            PERFORM WRITE-LINE.                                          
        END-ROUTINE-13.                                                  
            IF DELETE-COUNTER IS EQUAL TO ZERO                           
                MOVE "NO " TO ERROR-TOTAL  ELSE                          
                MOVE DELETE-COUNTER TO ERROR-TOTAL.                      
            MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   
            MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           
             IF   INSPECT-COUNTER EQUAL TO ZERO                          
                 MOVE "NO " TO ERROR-TOTAL                               
             ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   
             MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            
             MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          
            MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           
        WRITE-LINE.                                                      
            ADD 1 TO RECORD-COUNT.                                       
            IF RECORD-COUNT GREATER 50                                   
                MOVE DUMMY-RECORD TO DUMMY-HOLD                          
                MOVE SPACE TO DUMMY-RECORD                               
                WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  
                MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             
                MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     
                MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          
                MOVE DUMMY-HOLD TO DUMMY-RECORD                          
                MOVE ZERO TO RECORD-COUNT.                               
            PERFORM WRT-LN.                                              
        WRT-LN.                                                          
            WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               
            MOVE SPACE TO DUMMY-RECORD.                                  
        BLANK-LINE-PRINT.                                                
            PERFORM WRT-LN.                                              
        FAIL-ROUTINE.                                                    
            IF   COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE. 
            IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.
            MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 
            MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   
            MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   
            MOVE   SPACES TO INF-ANSI-REFERENCE.                         
            GO TO  FAIL-ROUTINE-EX.                                      
        FAIL-ROUTINE-WRITE.                                              
            MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         
            MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 
            MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. 
            MOVE   SPACES TO COR-ANSI-REFERENCE.                         
        FAIL-ROUTINE-EX. EXIT.                                           
        BAIL-OUT.                                                        
            IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   
            IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           
        BAIL-OUT-WRITE.                                                  
            MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  
            MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 
            MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   
            MOVE   SPACES TO INF-ANSI-REFERENCE.                         
        BAIL-OUT-EX. EXIT.                                               
        CCVS1-EXIT.                                                      
            EXIT.                                                        
        SECT-SM208A-001 SECTION.                                         
        REP-INIT-1.                                                      
            MOVE   "XII-6 3.2"  TO ANSI-REFERENCE.                       
            MOVE   "REP-TEST-1" TO PAR-NAME.                             
            MOVE    SPACE       TO WRK-XN-00001.                         
        REP-TEST-1-0.                                                    
            GO TO   REP-TEST-1-1.                                        
        REP-DELETE-1.                                                    
            PERFORM DE-LETE.                                             
            PERFORM PRINT-DETAIL.                                        
            GO TO   REP-INIT-2.                                          
        REP-TEST-1-1.                                                    
            MOVE   "*" TO WRK-XN-00001.                                  
            IF      WRK-XN-00001                     EQUAL "*"                                   
                    PERFORM PASS                                         
                    PERFORM PRINT-DETAIL                                 
            ELSE                                                         
                    MOVE   "REPLACE FAILED" TO RE-MARK                   
                    MOVE   "*"  TO CORRECT-X                             
                    MOVE    WRK-XN-00001 TO COMPUTED-X                   
                    PERFORM FAIL                                         
                    PERFORM PRINT-DETAIL.                                
        REP-INIT-2.                                                      
            MOVE   "XII-6 3.3 (SR5&6) AND XII-8 3.4(GR11)"               
                 TO ANSI-REFERENCE.                                      
            MOVE   "REP-TEST-2" TO PAR-NAME.                             
            MOVE    SPACES      TO WRK-XN-00322.                         
            MOVE    1 TO REC-CT.                                         
        REP-TEST-2-0.                                                    
            MOVE ""${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'} TO WRK-XN-00322.                                    
            GO TO   REP-TEST-2-1.                                        
        REP-DELETE-2.                                                    
            PERFORM DE-LETE.                                             
            PERFORM PRINT-DETAIL.                                        
            GO TO   REP-INIT-3.                                          
        REP-TEST-2-1.                                                    
            IF      WRK-XN-00322 =                      ""${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}            PERFORM PASS                                         
                    PERFORM PRINT-DETAIL                                 
            ELSE                                                         
                    MOVE   "REPLACING SINGLE CHARACTER BY 160 QUOTES"    
                         TO RE-MARK                                      
                    MOVE   ""${'"'}${'"'} TO CORRECT-X                             
                    MOVE    WRK-XN-00322-1 TO COMPUTED-X                 
                    PERFORM FAIL                                         
                    PERFORM PRINT-DETAIL                                 
                    ADD     1 TO REC-CT                                  
                    MOVE   ""${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'} TO CORRECT-X                 
                    MOVE    WRK-XN-00322-2 TO COMPUTED-X                 
                    PERFORM PRINT-DETAIL                                 
                    PERFORM WITH TEST AFTER                              
                       VARYING X1 FROM 1 BY 1                            
                          UNTIL X1 > 7                                   
                       ADD     1 TO REC-CT                               
                       MOVE ""${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}${'"'}   
                              TO CORRECT-X                               
                       MOVE  WRK-XN-00322-20 (X1) TO COMPUTED-X          
                       PERFORM PRINT-DETAIL                              
                    END-PERFORM.                                         
        REP-INIT-3.                                                      
            MOVE   "XII-6 3.3 (SR5&6) AND XII-8 3.4(GR11)"               
                 TO ANSI-REFERENCE.                                      
            MOVE   "REP-TEST-3" TO PAR-NAME.                             
            MOVE    SPACES      TO WRK-XN-00322.                         
            MOVE    1 TO REC-CT.                                         
        REP-TEST-3-0.                                                    
            MOVE "Y" TO WRK-XN-00322.                                    
            GO TO   REP-TEST-3-1.                                        
        REP-DELETE-3.                                                    
            PERFORM DE-LETE.                                             
            PERFORM PRINT-DETAIL.                                        
            GO TO   REP-INIT-4.                                          
        REP-TEST-3-1.                                                    
            IF      WRK-XN-00322-1 = "Y"                                 
                AND WRK-XN-00322-2-322 = SPACES                          
                    PERFORM PASS                                         
                    PERFORM PRINT-DETAIL                                 
            ELSE                                                         
                    MOVE   "REPLACING 160 QUOTES BY A SINGLE CHARACTER"  
                         TO RE-MARK                                      
                    MOVE   "Y"  TO CORRECT-X                             
                    MOVE    WRK-XN-00322-1 TO COMPUTED-X                 
                    PERFORM FAIL                                         
                    PERFORM PRINT-DETAIL                                 
                    ADD     1 TO REC-CT                                  
                    MOVE    SPACE TO CORRECT-X                           
                    MOVE    WRK-XN-00322-2 TO COMPUTED-X                 
                    PERFORM PRINT-DETAIL                                 
                    PERFORM WITH TEST AFTER                              
                            VARYING X1 FROM 1 BY 1                       
                              UNTIL X1 > 7                               
                            ADD     1 TO REC-CT                          
                            MOVE    SPACES TO CORRECT-X                  
                            MOVE    WRK-XN-00322-20 (X1) TO COMPUTED-X   
                            PERFORM PRINT-DETAIL                         
                    END-PERFORM.                                         
        REP-INIT-4.                                                      
            MOVE   "XII-8 3.4 (GR10)"  TO ANSI-REFERENCE.                
            MOVE   "REP-TEST-4" TO PAR-NAME.                             
            MOVE    SPACE       TO WRK-XN-00001.                         
        REP-TEST-4-0.                                                    
            GO TO   REP-TEST-4-1.                                        
        REP-DELETE-4.                                                    
            PERFORM DE-LETE.                                             
            PERFORM PRINT-DETAIL.                                        
            GO TO   REP-INIT-5.                                          
        REP-TEST-4-1.                                                    
            MOVE   "*" TO WRK-XN-00001.                                  
            IF      WRK-XN-00001 = "*"                                   
                    PERFORM PASS                                         
                    PERFORM PRINT-DETAIL                                 
            ELSE                                                         
                    MOVE   "REPLACE FAILED" TO RE-MARK                   
                    MOVE   "*"  TO CORRECT-X                             
                    MOVE    WRK-XN-00001 TO COMPUTED-X                   
                    PERFORM FAIL                                         
                    PERFORM PRINT-DETAIL.                                
        REP-INIT-5.                                                      
            MOVE   "XII-6 3.3 (SR4)" TO ANSI-REFERENCE.                  
            MOVE   "REP-TEST-5" TO PAR-NAME.                             
            MOVE    SPACES      TO WRK-XN-00020 WRK-XN-00001.            
        REP-TEST-5-0.                                                    
            MOVE   "AA BB CC DD EE FF GG" TO WRK-XN-00020.               
            IF WRK-XN-00020     EQUAL SPACES                             
                MOVE "*" TO WRK-XN-00001.                                
            GO TO   REP-TEST-5-1.                                        
        REP-DELETE-5.                                                    
            PERFORM DE-LETE.                                             
            PERFORM PRINT-DETAIL.                                        
            GO TO   REP-INIT-6.                                          
        REP-TEST-5-1.                                                    
            IF      WRK-XN-00001 EQUAL SPACES                            
                    PERFORM PASS                                         
                    PERFORM PRINT-DETAIL                                 
            ELSE                                                         
                    MOVE   "REPLACE FAILED" TO RE-MARK                   
                    PERFORM FAIL                                         
                    PERFORM PRINT-DETAIL.                                
        REP-INIT-6.                                                      
            MOVE   "XII-7/8 3.4 (GR7)" TO ANSI-REFERENCE.                
            MOVE   "REP-TEST-6" TO PAR-NAME.                             
        REP-TEST-6-0.                                                    
             MOVE                                                        
            "PASS"                                                       
            TO P-OR-F.                                                   
            GO TO   REP-TEST-6-1.                                        
        REP-DELETE-6.                                                    
            PERFORM DE-LETE.                                             
            PERFORM PRINT-DETAIL.                                        
            GO TO   REP-INIT-7.                                          
        REP-TEST-6-1.                                                    
            IF      P-OR-F = "PASS"                                      
                    PERFORM PASS                                         
                    PERFORM PRINT-DETAIL                                 
            ELSE                                                         
                    MOVE   "REPLACE FAILED" TO RE-MARK                   
                    MOVE   "PASS"  TO CORRECT-X                          
                    MOVE    P-OR-F TO COMPUTED-X                         
                    PERFORM FAIL                                         
                    PERFORM PRINT-DETAIL.                                
        REP-INIT-7.                                                      
            MOVE   "XII-8 3.4 (GR8)" TO ANSI-REFERENCE.                  
            MOVE   "REP-TEST-7" TO PAR-NAME.                             
            MOVE   "A" TO WS-A.                                          
            MOVE   "B" TO WS-B.                                          
            MOVE   "C" TO WS-C.                                          
            MOVE   "D" TO WS-D.                                          
            MOVE   "E" TO WS-E.                                          
            MOVE   "F" TO WS-F.                                          
        REP-TEST-7-0.                                                    
            MOVE WS-C TO WS-B.                                           
        REP-DELETE-7.                                                    
            PERFORM DE-LETE.                                             
            PERFORM PRINT-DETAIL.                                        
            GO TO   REP-INIT-8.                                          
        REP-TEST-7-1.                                                    
            IF      WS-B = "C"                                           
                    PERFORM PASS                                         
                    PERFORM PRINT-DETAIL                                 
            ELSE                                                         
                    MOVE   "REPLACE FAILED" TO RE-MARK                   
                    MOVE   "C"   TO CORRECT-X                            
                    MOVE    WS-B TO COMPUTED-X                           
                    PERFORM FAIL                                         
                    PERFORM PRINT-DETAIL.                                
        REP-INIT-8.                                                      
            MOVE   "XII-7 3.4 GR6(b)" TO ANSI-REFERENCE.                 
            MOVE   "REP-TEST-8" TO PAR-NAME.                             
            MOVE    SPACES      TO P-OR-F.                               
        REP-TEST-8-0.                                                    
             MOVE  , "PASS";      TO  P-OR-F.                            
            GO TO   REP-TEST-8-1.                                        
        REP-DELETE-8.                                                    
            PERFORM DE-LETE.                                             
            PERFORM PRINT-DETAIL.                                        
            GO TO   REP-INIT-9.                                          
        REP-TEST-8-1.                                                    
            IF      P-OR-F = "PASS"                                      
                    PERFORM PASS                                         
                    PERFORM PRINT-DETAIL                                 
            ELSE                                                         
                    MOVE   "REPLACE FAILED" TO RE-MARK                   
                    MOVE   "PASS"  TO CORRECT-X                          
                    MOVE    P-OR-F TO COMPUTED-X                         
                    PERFORM FAIL                                         
                    PERFORM PRINT-DETAIL.                                
        REP-INIT-9.                                                      
            MOVE   "XII-7 3.4 GR4" TO ANSI-REFERENCE.                    
            MOVE   "REP-TEST-9" TO PAR-NAME.                             
            MOVE   "FAIL"       TO P-OR-F.                               
        REP-TEST-9-0.                                                    
            MOVE "PASS" TO P-OR-F.                                       
            GO TO   REP-TEST-9-1.                                        
        REP-DELETE-9.                                                    
            PERFORM DE-LETE.                                             
            PERFORM PRINT-DETAIL.                                        
            GO TO   CCVS-EXIT.                                           
        REP-TEST-9-1.                                                    
            IF      P-OR-F = "PASS"                                      
                    PERFORM PASS                                         
                    PERFORM PRINT-DETAIL                                 
            ELSE                                                         
                    MOVE   "REPLACE FAILED" TO RE-MARK                   
                    MOVE   "PASS"  TO CORRECT-X                          
                    MOVE    P-OR-F TO COMPUTED-X                         
                    PERFORM FAIL                                         
                    PERFORM PRINT-DETAIL.                                
        CCVS-EXIT SECTION.                                               
        CCVS-999999.                                                     
            GO TO CLOSE-FILES.                                           
        """.trimIndent()
}
