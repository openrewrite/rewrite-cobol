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

import org.assertj.core.api.Assertions
import org.junit.jupiter.api.Test
import org.openrewrite.ExecutionContext
import org.openrewrite.InMemoryExecutionContext
import org.openrewrite.PrintOutputCapture
import org.openrewrite.cobol.Assertions.cobolCopy
import org.openrewrite.cobol.CobolVisitor
import org.openrewrite.cobol.internal.IbmAnsi85
import org.openrewrite.internal.EncodingDetectingInputStream
import org.openrewrite.test.RecipeSpec
import org.openrewrite.test.RewriteTest
import java.nio.file.Files
import java.nio.file.Paths

class CobolParserReplaceTest : RewriteTest {

    companion object {
        val dialect = IbmAnsi85()

        private val userDir = System.getProperty("user.dir")
        private const val nistPath = "/src/test/resources/gov/nist/"
        fun getNistSource(bookName: String): String {
            val path = Paths.get(userDir + nistPath + bookName)
            val inputStream = Files.newInputStream(path)
            val encoding = EncodingDetectingInputStream(inputStream)
            return encoding.readFully()
        }
    }

    override fun defaults(spec: RecipeSpec) {
        spec.recipe(RewriteTest.toRecipe {
                object : CobolVisitor<ExecutionContext>() {
                    override fun visitSpace(space: Space, p: ExecutionContext): Space {
                        val whitespace = space.whitespace.trim()
                        if (!(dialect.separators.contains("$whitespace ") || whitespace.isEmpty())) {
                            return space.withWhitespace("(~~>${space.whitespace}<~~)")
                        }
                        return space
                    }
                }
            })
    }

    @Test
    fun sm201A() = rewriteRun(
        cobolCopy(getNistSource("SM201A.CBL")) { spec ->
            spec.afterRecipe { cu ->
                val outputCapture = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                CobolParserCopyTest.printer.visit(cu, outputCapture)
                val result = outputCapture.getOut().trimIndent()
                Assertions.assertThat(result).isEqualTo(
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
                )
            }
        }
    )

    @Test
    fun sm202A() = rewriteRun(
        cobolCopy(getNistSource("SM202A.CBL")) { spec ->
            spec.afterRecipe { cu ->
                val outputCapture = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                CobolParserCopyTest.printer.visit(cu, outputCapture)
                val result = outputCapture.getOut().trimIndent()
                Assertions.assertThat(result).isEqualTo(
                    """
                    """.trimIndent()
                )
            }
        }
    )

    @Test
    fun sm203A() = rewriteRun(
        cobolCopy(getNistSource("SM203A.CBL")) { spec ->
            spec.afterRecipe { cu ->
                val outputCapture = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                CobolParserCopyTest.printer.visit(cu, outputCapture)
                val result = outputCapture.getOut().trimIndent()
                Assertions.assertThat(result).isEqualTo(
                    """
                    """.trimIndent()
                )
            }
        }
    )

    @Test
    fun sm205A() = rewriteRun(
        cobolCopy(getNistSource("SM205A.CBL")) { spec ->
            spec.afterRecipe { cu ->
                val outputCapture = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                CobolParserCopyTest.printer.visit(cu, outputCapture)
                val result = outputCapture.getOut().trimIndent()
                Assertions.assertThat(result).isEqualTo(
                    """
                    """.trimIndent()
                )
            }
        }
    )

    @Test
    fun sm206A() = rewriteRun(
        cobolCopy(getNistSource("SM206A.CBL")) { spec ->
            spec.afterRecipe { cu ->
                val outputCapture = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                CobolParserCopyTest.printer.visit(cu, outputCapture)
                val result = outputCapture.getOut().trimIndent()
                Assertions.assertThat(result).isEqualTo(
                    """
                    """.trimIndent()
                )
            }
        }
    )

    @Test
    fun sm208A() = rewriteRun(
        cobolCopy(getNistSource("SM208A.CBL")) { spec ->
            spec.afterRecipe { cu ->
                val outputCapture = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                CobolParserCopyTest.printer.visit(cu, outputCapture)
                val result = outputCapture.getOut().trimIndent()
                Assertions.assertThat(result).isEqualTo(
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
                )
            }
        }
    )
}
