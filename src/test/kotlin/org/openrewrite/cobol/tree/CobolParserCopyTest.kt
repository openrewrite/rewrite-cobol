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
import org.junit.jupiter.api.Test
import org.openrewrite.ExecutionContext
import org.openrewrite.InMemoryExecutionContext
import org.openrewrite.PrintOutputCapture
import org.openrewrite.cobol.Assertions.cobolCopy
import org.openrewrite.cobol.CobolIsoVisitor
import org.openrewrite.cobol.internal.CobolPrinter
import org.openrewrite.cobol.internal.IbmAnsi85
import org.openrewrite.internal.EncodingDetectingInputStream
import org.openrewrite.test.RecipeSpec
import org.openrewrite.test.RewriteTest
import org.openrewrite.test.RewriteTest.toRecipe
import java.nio.file.Files
import java.nio.file.Paths

class CobolParserCopyTest : RewriteTest {

    companion object {
        val dialect = IbmAnsi85()
        val printer =
            CobolPrinter<ExecutionContext>(
                false,
                false
            )

        private val userDir = System.getProperty("user.dir")
        private val nistPath = "/src/test/resources/gov/nist/"
        fun getNistSource(sourceName: String): String {
            val path = Paths.get(userDir + nistPath + sourceName)
            val inputStream = Files.newInputStream(path)
            val encoding = EncodingDetectingInputStream(inputStream)
            return encoding.readFully()
        }
    }

    override fun defaults(spec: RecipeSpec) {
        spec.recipe(toRecipe {
            object : CobolIsoVisitor<ExecutionContext>() {
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
    fun sm101A() = rewriteRun(
        cobolCopy(getNistSource("SM101A.CBL")) { spec ->
            spec.afterRecipe { cu ->
                val outputCapture = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                printer.visit(cu, outputCapture)
                val result = outputCapture.getOut().trimIndent()
                assertThat(result).isEqualTo(
                    """
                        IDENTIFICATION DIVISION.                                         
                        PROGRAM-ID.                                                      
                            SM101A.                                                      
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
                            02 TST-FLD-1 PICTURE 9(5).                                   
                            02 FILLER    PICTURE X(115).                                 
                        WORKING-STORAGE SECTION.                                         
                        77  RCD-1            PICTURE 9(5) VALUE 97523.                            
                        77  RCD-2    PICTURE 9(5) VALUE 23497.                           
                        77  RCD-3 PICTURE 9(5) VALUE 10901.                              
                        77    RCD-4    PIC 9(5) VALUE 02734.                               
                        77  RCD-5    PICTURE IS 99999 VALUE IS                           
                                              14003.                                     
                        77  RCD-6 PICTURE 9(5) VALUE 19922.                              
                        77    RCD-7 PIC 9(5)                                               
                           VALUE 3543.                                    
                        77  COPYSECT-1 PICTURE 9(5) VALUE 72459.                         
                        77  COPYSECT-2 PICTURE 9(5) VALUE 12132.                         
                        77  COPYSECT-3 PICTURE X(5) VALUE "TSTLI".                       
                        77  COPYSECT-4 PICTURE X(5) VALUE "BCOPY".                       
                        01  GRP-001.                                                     
                            02 WRK-DS-05V00 PIC S9(5)                                    
                                     VALUE                                               
                                          IS                                             
                                                   ZERO.                                 
                        77  PROC-1 PICTURE 999 VALUE 123.                                
                        77  PROC-2 PICTURE 999 VALUE 456.                                
                        77  WSTR-1  PICTURE X(3) VALUE "ABC".                            
                        01  WSTR-2.                                                      
                            02 WSTR-2A PICTURE X(3) VALUE                              "ABC".                                                        
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
                            "SM101A".                                                    
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
                        SM101A-INIT.                                                     
                            OPEN     OUTPUT TEST-FILE.                                   
                            MOVE     "OUTPUT OF SM101A IS USED AS" TO RE-MARK.           
                            PERFORM  PRINT-DETAIL.                                       
                            MOVE     "INPUT FOR SM102A."           TO RE-MARK.           
                            PERFORM  PRINT-DETAIL.                                       
                            MOVE     "COPY ---" TO FEATURE.                              
                            PERFORM  PRINT-DETAIL.                                       
                        WORKING-STORAGE-TEST SECTION.                                    
                        COPY-TEST-1.                                                     
                            IF       WSTR-1 EQUAL TO WSTR-2                              
                                     PERFORM PASS GO TO COPY-WRITE-1.                    
                            GO       TO COPY-FAIL-1.                                     
                        COPY-DELETE-1.                                                   
                            PERFORM  DE-LETE.                                            
                            GO       TO COPY-WRITE-1.                                    
                        COPY-FAIL-1.                                                     
                            MOVE     WSTR-2 TO COMPUTED-A.                               
                            MOVE     "ABC" TO CORRECT-A                                  
                            PERFORM  FAIL.                                               
                        COPY-WRITE-1.                                                    
                            MOVE     "  WKNG-STORAGE ENTRY" TO FEATURE                   
                            MOVE     "COPY-TEST-1 " TO PAR-NAME.                         
                            PERFORM  PRINT-DETAIL.                                       
                        PARAGRAPH-TEST SECTION.                                          
                        COPY-TEST-2.                                                     
                            MOVE PROC-1 TO PROC-2.                                       
                        COPY-TESTT-2.                                                    
                            IF       PROC-1 EQUAL TO PROC-2                              
                                     PERFORM PASS GO TO COPY-WRITE-2.                    
                            GO       TO COPY-FAIL-2.                                     
                        COPY-DELETE-2.                                                   
                            PERFORM  DE-LETE.                                            
                            GO       TO COPY-WRITE-2.                                    
                        COPY-FAIL-2.                                                     
                            MOVE     PROC-2 TO COMPUTED-N.                               
                            MOVE     123 TO CORRECT-N.                                   
                            PERFORM  FAIL.                                               
                        COPY-WRITE-2.                                                    
                            MOVE     "  PROCEDURE" TO FEATURE                            
                            MOVE     "COPY-TEST-2 " TO PAR-NAME.                         
                            PERFORM  PRINT-DETAIL.                                       
                        SECTION-TEST SECTION.                                            
                        SECT-COPY-1.                                                     
                            MOVE     95427 TO COPYSECT-1.                                
                        SECT-COPY-2.                                                     
                            MOVE     23121 TO COPYSECT-2.                                
                        SECT-COPY-3.                                                     
                            MOVE     "LIBCO" TO COPYSECT-3.                              
                        SECT-COPY-4.                                                     
                            MOVE     "PYTST" TO COPYSECT-4.                              
                        SECT-COPY-1.                                                     
                            MOVE     95427 TO COPYSECT-1.                                
                        SECT-COPY-2.                                                     
                            MOVE     23121 TO COPYSECT-2.                                
                        SECT-COPY-3.                                                     
                            MOVE     "LIBCO" TO COPYSECT-3.                              
                        SECT-COPY-4.                                                     
                            MOVE     "PYTST" TO COPYSECT-4.                              
                        COPY-INIT-A.                                                     
                            MOVE     "  SECTION" TO FEATURE.                             
                        COPY-TEST-3.                                                     
                            IF       COPYSECT-1 EQUAL TO 95427                           
                                     PERFORM PASS GO TO COPY-WRITE-3.                    
                            GO       TO COPY-FAIL-3.                                     
                        COPY-DELETE-3.                                                   
                            PERFORM  DE-LETE.                                            
                            GO       TO COPY-WRITE-3.                                    
                        COPY-FAIL-3.                                                     
                            MOVE     COPYSECT-1 TO COMPUTED-N.                           
                            MOVE     95427   TO CORRECT-N.                               
                            PERFORM  FAIL.                                               
                        COPY-WRITE-3.                                                    
                            MOVE     "COPY-TEST-3 " TO PAR-NAME.                         
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-4.                                                     
                            IF       COPYSECT-2 EQUAL TO 23121                           
                                     PERFORM PASS GO TO COPY-WRITE-4.                    
                            GO       TO COPY-FAIL-4.                                     
                        COPY-DELETE-4.                                                   
                            PERFORM  DE-LETE.                                            
                            GO       TO COPY-WRITE-4.                                    
                        COPY-FAIL-4.                                                     
                            MOVE     COPYSECT-2 TO COMPUTED-N.                           
                            MOVE     23121   TO CORRECT-N.                               
                            PERFORM  FAIL.                                               
                        COPY-WRITE-4.                                                    
                            MOVE     "COPY-TEST-4 " TO PAR-NAME.                         
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-5.                                                     
                            IF       COPYSECT-3 EQUAL TO "LIBCO"                         
                                     PERFORM PASS GO TO COPY-WRITE-5.                    
                            GO       TO COPY-FAIL-5.                                     
                        COPY-DELETE-5.                                                   
                            PERFORM  DE-LETE.                                            
                            GO       TO COPY-WRITE-5.                                    
                        COPY-FAIL-5.                                                     
                            MOVE     COPYSECT-3 TO COMPUTED-A.                           
                            MOVE     "LIBCO" TO CORRECT-A.                               
                            PERFORM  FAIL.                                               
                        COPY-WRITE-5.                                                    
                            MOVE     "COPY-TEST-5 " TO PAR-NAME.                         
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-6.                                                     
                            IF       COPYSECT-4 EQUAL TO "PYTST"                         
                                     PERFORM PASS GO TO COPY-WRITE-6.                    
                            GO       TO COPY-FAIL-6.                                     
                        COPY-DELETE-6.                                                   
                            PERFORM  DE-LETE.                                            
                            GO       TO COPY-WRITE-6.                                    
                        COPY-FAIL-6.                                                     
                            MOVE     COPYSECT-4 TO COMPUTED-A.                           
                            MOVE     "PYTST" TO CORRECT-A.                               
                            PERFORM  FAIL.                                               
                        COPY-WRITE-6.                                                    
                            MOVE     "COPY-TEST-6 " TO PAR-NAME.                         
                            PERFORM  PRINT-DETAIL.                                       
                        BUILD SECTION.                                                   
                        COPY-TEST-7.                                                     
                            MOVE     RCD-1 TO TST-FLD-1.                                 
                            WRITE    TST-TEST.                                           
                            MOVE     RCD-2 TO TST-FLD-1.                                 
                            WRITE    TST-TEST.                                           
                            MOVE     RCD-3 TO TST-FLD-1.                                 
                            WRITE    TST-TEST.                                           
                            MOVE     RCD-4 TO TST-FLD-1.                                 
                            WRITE    TST-TEST.                                           
                            MOVE     RCD-5 TO TST-FLD-1.                                 
                            WRITE    TST-TEST.                                           
                            MOVE     RCD-6 TO TST-FLD-1.                                 
                            WRITE    TST-TEST.                                           
                            MOVE     RCD-7 TO TST-FLD-1.                                 
                            WRITE    TST-TEST.                                           
                            PERFORM  PASS.                                               
                            GO       TO COPY-WRITE-7.                                    
                        COPY-DELETE-7.                                                   
                            PERFORM  DE-LETE.                                            
                        COPY-WRITE-7.                                                    
                            MOVE     "  FILE DESCRIPTION" TO FEATURE.                    
                            MOVE     "COPY-TEST-7" TO PAR-NAME.                          
                            MOVE     "OUTPUT CHECKED IN SM102A" TO RE-MARK.              
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-8.                                                     
                            ADD         RCD-1                                                   
                         TO WRK-DS-05V00.                         
                            IF       WRK-DS-05V00 EQUAL TO 97523                         
                                    PERFORM PASS                                         
                                    GO TO COPY-WRITE-8.                                  
                            GO TO    COPY-FAIL-8.                                        
                        COPY-DELETE-8.                                                   
                            PERFORM DE-LETE.                                             
                            GO TO    COPY-WRITE-8.                                       
                        COPY-FAIL-8.                                                     
                            MOVE    WRK-DS-05V00 TO COMPUTED-N.                          
                            MOVE    97523        TO CORRECT-N.                           
                            PERFORM FAIL.                                                
                        COPY-WRITE-8.                                                    
                            MOVE     "COPY-TEST-8" TO PAR-NAME.                          
                            PERFORM PRINT-DETAIL.                                        
                            CLOSE    TEST-FILE.                                          
                        CCVS-EXIT SECTION.                                               
                        CCVS-999999.                                                     
                            GO TO CLOSE-FILES.                                           
                    """.trimIndent())
            }
        }
    )

    @Test
    fun sm103A() = rewriteRun(
        cobolCopy(getNistSource("SM103A.CBL")) { spec ->
            spec.afterRecipe { cu ->
                val outputCapture = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                printer.visit(cu, outputCapture)
                val result = outputCapture.getOut().trimIndent()
                assertThat(result).isEqualTo(
                    """
                        IDENTIFICATION DIVISION.                                         
                        PROGRAM-ID.                                                      
                            SM103A.                                                      
                        SECURITY.                                                        
                            COPY K3SNA.                                                  
                        ENVIRONMENT DIVISION.                                            
                        CONFIGURATION SECTION.                                           
                        SOURCE-COMPUTER.    XXXXX082.                                                    
                        OBJECT-COMPUTER.    XXXXX083.                                                    
                        SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.                                      
                        INPUT-OUTPUT SECTION.                                            
                        FILE-CONTROL.    SELECT TEST-FILE ASSIGN TO                                   
                            XXXXP001.                                                    
                            SELECT TEST-FILE2 ASSIGN TO                                  
                            XXXXP002.                                                    
                            SELECT PRINT-FILE ASSIGN TO                                  
                            XXXXX055.                                                    
                        I-O-CONTROL.    SAME AREA FOR TEST-FILE                                      
                                          TEST-FILE2.                                    
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
                            DATA RECORD TEST-REC.                                        
                        01  TEST-REC.                                                    
                            02  TST-FLD-1 PICTURE 9(5).                                  
                            02  TST-FLD-2 PICTURE X(13).                                 
                            02  FILLER PICTURE X(102).                                   
                        FD  TEST-FILE2                                                   
                            LABEL RECORD STANDARD                                        
                            VALUE OF                                                     
                            XXXXX074                                                     
                            IS                                                           
                            XXXXX076                                                     
                            DATA RECORD TEST-REC2.                                       
                        01  TEST-REC2.                                                   
                            02  TST-FLD-3 PICTURE 9(5).                                  
                            02  TST-FLD-4 PICTURE X(13).                                 
                            02  FILLER PICTURE X(102).                                   
                        WORKING-STORAGE SECTION.                                         
                        77  RCD-1 PICTURE 9(5) VALUE 97532.                              
                        77  RCD-2 PICTURE 9(5) VALUE 23479.                              
                        77  RCD-3 PICTURE 9(5) VALUE 10901.                              
                        77  RCD-4 PICTURE 9(5) VALUE 02734.                              
                        77  RCD-5 PICTURE 9(5) VALUE 14003.                              
                        77  RCD-6 PICTURE 9(5) VALUE 19922.                              
                        77  RCD-7 PICTURE 9(5) VALUE 03543.                              
                        01  S-N-1 PICTURE 9(8)V99 VALUE IS 12345678,91.                  
                        01  S-N-2 PICTURE ZZ.ZZZ.ZZZ,99.                                 
                        01  WRK-DU-9                    PIC 9          VALUE ZERO.       
                        01  WRK-DU-99                   PIC 99         VALUE ZERO.       
                        01  WRK-DU-99-LONGER            PIC 99         VALUE ZERO.       
                        01  WRK-DU-00001                PIC 9.                           
                        01  WRK-XN-00322                PIC X(322).                      
                        01  FILLER REDEFINES WRK-XN-00322.                               
                          03  WRK-XN-00322-1         PIC X.                              
                          03  WRK-XN-00322-2-322.                                        
                            05  WRK-XN-00322-2-3     PIC X.                              
                            05  WRK-XN-00322-20      PIC X(20)                           
                                                     OCCURS 16                           
                                                     INDEXED BY X1.                      
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
                            "SM103A".                                                    
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
                        SM103A-INIT.                                                     
                            MOVE     "ALL TESTS IN SM103A PRODUCE" TO RE-MARK.           
                            PERFORM  PRINT-DETAIL.                                       
                            MOVE     "OUTPUT CHECKED IN SM104A."   TO RE-MARK.           
                            PERFORM  PRINT-DETAIL.                                       
                            MOVE     "COPY ---" TO FEATURE.                              
                            PERFORM  PRINT-DETAIL.                                       
                        SPECIAL-NAMES-TEST SECTION.                                      
                        COPY-TEST-1.                                                     
                            MOVE     S-N-1 TO S-N-2.                                     
                            PERFORM  PASS.                                               
                            GO       TO COPY-WRITE-1.                                    
                        COPY-DELETE-1.                                                   
                            PERFORM  DE-LETE.                                            
                        COPY-WRITE-1.                                                    
                            MOVE     "  DEC POINT IS COMMA" TO FEATURE.                  
                            MOVE     "COPY-TEST-1 " TO PAR-NAME.                         
                            PERFORM  PRINT-DETAIL.                                       
                        BUILD SECTION.                                                   
                        COPY-TEST-2.                                                     
                            OPEN     OUTPUT TEST-FILE.                                   
                            MOVE     S-N-2 TO TST-FLD-2.                                 
                            MOVE     RCD-1 TO TST-FLD-1.                                 
                            WRITE    TEST-REC.                                           
                            MOVE     RCD-2 TO TST-FLD-1.                                 
                            WRITE    TEST-REC.                                           
                            MOVE     RCD-3 TO TST-FLD-1.                                 
                            WRITE    TEST-REC.                                           
                            MOVE     RCD-4 TO TST-FLD-1.                                 
                            WRITE    TEST-REC.                                           
                            MOVE     RCD-5 TO TST-FLD-1.                                 
                            WRITE    TEST-REC.                                           
                            MOVE     RCD-6 TO TST-FLD-1.                                 
                            WRITE    TEST-REC.                                           
                            MOVE     RCD-7 TO TST-FLD-1.                                 
                            WRITE    TEST-REC.                                           
                            CLOSE    TEST-FILE.                                          
                            OPEN OUTPUT TEST-FILE2.                                      
                            MOVE ZERO TO TST-FLD-3.                                      
                            MOVE "DDDDD" TO TST-FLD-4.                                   
                            WRITE TEST-REC2.                                             
                            CLOSE TEST-FILE2.                                            
                            PERFORM  PASS.                                               
                            GO       TO COPY-WRITE-2.                                    
                        COPY-DELETE-2.                                                   
                            PERFORM DE-LETE.                                             
                        COPY-WRITE-2.                                                    
                            MOVE   "  ENVIR DIV ENTRIES" TO FEATURE.                     
                            MOVE   "COPY-TEST-2 " TO PAR-NAME.                           
                            PERFORM PRINT-DETAIL.                                        
                        COPY-TEST-3.                                                     
                            MOVE   "XII-2 2.3 SR8" TO ANSI-REFERENCE.                    
                            MOVE   "COPY-TEST-3"   TO PAR-NAME.                          
                            MOVE    8  TO WRK-DU-00001.                                  
                            GO TO   COPY-TEST-3-0.                                       
                        COPY-DELETE-3.                                                   
                            PERFORM DE-LETE.                                             
                            PERFORM PRINT-DETAIL.                                        
                            GO TO   COPY-INIT-4.                                         
                        COPY-TEST-3-0.                                                   
                            IF      WRK-DU-00001 =                                       
                                                    8                                    
                                    PERFORM PASS                                         
                                    PERFORM PRINT-DETAIL                                 
                            ELSE                                                         
                                    MOVE   "COPYING SINGLE CHARACTER FAILED"             
                                         TO RE-MARK                                      
                                    MOVE    8   TO CORRECT-N                             
                                    MOVE    WRK-DU-00001 TO COMPUTED-N                   
                                    PERFORM FAIL                                         
                                    PERFORM PRINT-DETAIL.                                
                        COPY-INIT-4.                                                     
                            MOVE   "XII-2 2.3 (SR8) AND XII-5 2.4(GR11)"                 
                                 TO ANSI-REFERENCE.                                      
                            MOVE   "COPY-TEST-4" TO PAR-NAME.                            
                            MOVE    SPACES      TO WRK-XN-00322.                         
                            MOVE    1 TO REC-CT.                                         
                            GO TO   COPY-TEST-4-0.                                       
                        COPY-DELETE-4.                                                   
                            PERFORM DE-LETE.                                             
                            PERFORM PRINT-DETAIL.                                        
                            GO TO   CCVS-EXIT.                                           
                        COPY-TEST-4-0.                                                   
                            MOVE 1 TO WRK-DU-9, ADD 1 TO WRK-DU-9, ADD 1 TO WRK-DU-9, ADD
                             1 TO WRK-DU-99, ADD 1 TO WRK-DU-9, ADD 1 TO WRK-DU-99, ADD 1
                             TO WRK-DU-99, ADD 1 TO WRK-DU-99, ADD 1 TO WRK-DU-99, ADD 1 
                            TO WRK-DU-9, ADD 1 TO WRK-DU-9, ADD 1 TO WRK-DU-99, ADD 1 TO 
                            WRK-DU-99, ADD 1 TO WRK-DU-99, ADD 1 TO WRK-DU-99, ADD 10 TO 
                            WRK-DU-99-LONGER.                                            
                        COPY-TEST-4-1.                                                   
                            MOVE   "COPY-TEST-4-1" TO PAR-NAME.                          
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
                        COPY-TEST-4-2.                                                   
                            MOVE   "COPY-TEST-4-2" TO PAR-NAME.                          
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
                        COPY-TEST-4-3.                                                   
                            MOVE   "COPY-TEST-4-3" TO PAR-NAME.                          
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
                )
            }
        }
    )

    @Test
    fun sm105A() = rewriteRun(
        cobolCopy(getNistSource("SM105A.CBL")) { spec ->
            spec.afterRecipe { cu ->
                val outputCapture = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                printer.visit(cu, outputCapture)
                val result = outputCapture.getOut().trimIndent()
                assertThat(result).isEqualTo(
                    """
                        IDENTIFICATION DIVISION.                                         
                        PROGRAM-ID.                                                      
                            SM105A.                                                      
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
                            SELECT SORTFILE-1E ASSIGN TO                                 
                            XXXXX027.                                                    
                            SELECT SORTOUT-1E ASSIGN TO                                  
                            XXXXX001.                                                    
                        DATA DIVISION.                                                   
                        FILE SECTION.                                                    
                        FD  PRINT-FILE.                                                  
                        01  PRINT-REC PICTURE X(120).                                    
                        01  DUMMY-RECORD PICTURE X(120).                                 
                        SD  SORTFILE-1E    DATA RECORD S-RECORD.                                        
                        01  S-RECORD.                                                    
                            02  KEYS-GROUP.                                              
                                03  KEY-1 PICTURE 9.                                     
                                03  KEY-2 PICTURE 99.                                    
                                03  KEY-3 PICTURE 999.                                   
                                03  KEY-4 PICTURE 9999.                                  
                                03  KEY-5 PICTURE 99999.                                 
                            02 RDF-KEYS REDEFINES KEYS-GROUP PICTURE 9(15).              
                            02 FILLER PICTURE X(105).                                    
                        FD  SORTOUT-1E                                                   
                            BLOCK CONTAINS 10 RECORDS                                    
                            LABEL RECORDS ARE STANDARD                                   
                            VALUE OF                                                     
                            XXXXX074                                                     
                            IS                                                           
                            XXXXX075                                                     
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
                          02  FILLER                        PIC X(41)  VALUE SPACE.      
                          02  FILLER                        PIC X(39)  VALUE             
                                   "CCVS85  NCC COPY - NOT FOR DISTRIBUTION".            
                          02  FILLER                        PIC X(40)  VALUE SPACE.      
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
                            02 FILLER                       PIC X(45)  VALUE             
                                   " NTIS DISTRIBUTION COBOL 85".                        
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
                            "SM105A".                                                    
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
                            SORT SORTFILE-1E                                             
                            ON ASCENDING KEY KEY-1                                       
                            ON DESCENDING KEY KEY-2                                      
                            ON ASCENDING KEY KEY-3                                       
                            DESCENDING KEY-4 KEY-5                                       
                            INPUT PROCEDURE IS INSORT                                    
                            OUTPUT PROCEDURE IS OUTP1 THRU OUTP3.                        
                        I-2.                                                             
                            GO TO    CLOSE-FILES.                                        
                        INSORT SECTION.                                                  
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
                            IF WKEY-4 EQUAL TO C2                                        
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
                        SM105-INIT.                                                      
                            OPEN     OUTPUT SORTOUT-1E.                                  
                            MOVE     "COPY SORT DESCR" TO FEATURE.                       
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
                        OUTP2 SECTION.                                                   
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
                            MOVE     "COPY-TEST-4 " TO PAR-NAME.                         
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
                        COPY-TEST-9.                                                     
                            RETURN   SORTFILE-1E END                                     
                                     PERFORM PASS-1 GO TO COPY-WRITE-9.                  
                            PERFORM  FAIL-1.                                             
                            MOVE     RDF-KEYS TO COMPUTED-18V0.                          
                            MOVE     "END OF FILE NOT FOUND" TO RE-MARK.                 
                            GO       TO COPY-WRITE-9.                                    
                        COPY-DELETE-9.                                                   
                            PERFORM  DE-LETE-1.                                          
                        COPY-WRITE-9.                                                    
                            MOVE     "COPY-TEST-9 " TO PAR-NAME.                         
                            PERFORM  PRINT-DETAIL-1.                                     
                        OUTP3 SECTION.                                                   
                        RET-0.                                                           
                            CLOSE    SORTOUT-1E.                                         
                            GO       TO LIB1E-EXIT.                                      
                        RET-1.                                                           
                            RETURN   SORTFILE-1E RECORD AT END GO TO BAD-FILE.           
                            MOVE     S-RECORD TO SORTED.                                 
                            WRITE    SORTED.                                             
                        RET-2.                                                           
                            RETURN   SORTFILE-1E           END GO TO BAD-FILE.           
                            MOVE     S-RECORD TO SORTED.                                 
                            WRITE    SORTED.                                             
                        BAD-FILE.                                                        
                            PERFORM  FAIL-1.                                             
                            MOVE     "BAD-FILE" TO PAR-NAME.                             
                            MOVE     "EOF PREMATURELY FOUND" TO RE-MARK.                 
                            PERFORM  PRINT-DETAIL-1.                                     
                            CLOSE    SORTOUT-1E.                                         
                            GO TO    LIB1E-EXIT.                                         
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
                        LIB1E-EXIT.                                                      
                            EXIT.                                                        
                    """.trimIndent()
                )
            }
        }
    )

    @Test
    fun sm106A() = rewriteRun(
        cobolCopy(getNistSource("SM106A.CBL")) { spec ->
            spec.afterRecipe { cu ->
                val outputCapture = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                printer.visit(cu, outputCapture)
                val result = outputCapture.getOut().trimIndent()
                assertThat(result).isEqualTo(
                    """
                        IDENTIFICATION DIVISION.                                         
                        PROGRAM-ID.                                                      
                            SM106A.                                                      
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
                            "K6SCA".                                                     
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
                            IF RECORD-COUNT GREATER 42                                   
                                MOVE DUMMY-RECORD TO DUMMY-HOLD                          
                                MOVE SPACE TO DUMMY-RECORD                               
                                WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  
                                MOVE CCVS-H-1  TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES    
                                MOVE CCVS-H-2A TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES    
                                MOVE CCVS-H-2B TO DUMMY-RECORD PERFORM WRT-LN 3 TIMES    
                                MOVE CCVS-H-3  TO DUMMY-RECORD PERFORM WRT-LN 3 TIMES    
                                MOVE CCVS-C-1  TO DUMMY-RECORD PERFORM WRT-LN            
                                MOVE CCVS-C-2  TO DUMMY-RECORD PERFORM WRT-LN            
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
                            IF     COMPUTED-X NOT EQUAL TO SPACE                         
                                   GO TO   FAIL-ROUTINE-WRITE.                           
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
                        LB106A-INIT SECTION.                                             
                        LB106A-001.                                                      
                            MOVE  " REGARDLESS OF WHAT APPEARS ABOVE OR BELOW, THIS IS THE REPORT FOR SM106A" TO PRINT-REC.                    
                            PERFORM WRITE-LINE.                                          
                            PERFORM BLANK-LINE-PRINT.                                    
                            MOVE     " THE PRESENCE OF THIS MESSAGE INDICATES THAT TEXT FOR ALL 3 DIVISIONS CAN BE GENERATED BY ONE COPY STATEMENT."             TO PRINT-REC.                                       
                            PERFORM       WRITE-LINE.                                    
                            PERFORM       INSPT.                                         
                        CCVS-EXIT SECTION.                                               
                        CCVS-999999.                                                     
                            GO TO CLOSE-FILES.                                           
                    """.trimIndent()
                )
            }
        }
    )

    @Test
    fun sm107A() = rewriteRun(
        cobolCopy(getNistSource("SM107A.CBL")) { spec ->
            spec.afterRecipe { cu ->
                val outputCapture = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                printer.visit(cu, outputCapture)
                val result = outputCapture.getOut().trimIndent()
                assertThat(result).isEqualTo(
                    """
                        IDENTIFICATION DIVISION.                                         
                        PROGRAM-ID.                                                      
                            SM107A.                                                      
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
                            "SM107A".                                                    
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
                        WARNING-MSG.                                                     
                            MOVE " IF NO OTHER REPORT LINES APPEAR BELOW, ""COPY K7SEA"" FAILED." TO PRINT-REC.                                 
                            PERFORM WRITE-LINE.                                          
                        COPY-TEST-1.                                                     
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-1  " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-2.                                                     
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-2  " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-3.                                                     
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-3  " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-4.                                                     
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-4  " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-5.                                                     
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-5  " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-6.                                                     
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-6  " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-7.                                                     
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-7  " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-8.                                                     
                            MOVE     "COPY-TEST-8  " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-9.                                                     
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-9  " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-10.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-10 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-11.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-11 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-12.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-12 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-13.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-13 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-14.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-14 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-15.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-15 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-16.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-16 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-17.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-17 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-18.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-18 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-19.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-19 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-20.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-20 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-21.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-21 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-2I.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-22 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-23.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-23 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-24.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-24 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-25.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-25 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-26.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-26 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-27.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-27 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-28.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-28 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-29.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-29 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-30.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-30 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-31.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-31 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-32.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-32 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-33.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-33 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-34.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-34 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-35.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-35 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-36.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-36 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-37.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-37 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-38.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-38 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-39.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-39 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-40.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-40 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-41.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-41 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-42.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-42 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-43.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-43 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-44.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-44 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-45.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-45 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-46.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-46 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-47.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-47 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-48.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-48 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-49.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-49 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-50.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-50 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-51.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-51 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-52.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-52 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-53.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-53 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-54.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-54 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-55.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-55 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-56.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-56 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-57.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-57 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-58.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-58 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-59.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-59 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-60.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-60 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-61.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-61 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-62.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-62 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-63.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-63 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-64.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-64 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-65.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-65 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-66.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-66 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-67.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-67 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-68.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-68 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-69.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-69 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-70.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-70 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-71.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-71 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-72.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-72 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-73.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-73 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-74.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-74 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-75.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-75 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-76.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-76 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-77.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-77 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-78.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-78 " TO PAR-NAME.                        
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-79.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-79 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-80.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-80 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-81.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-81 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-82.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-82 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-83.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-83 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-84.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-84 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-85.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-85 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-86.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-86 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-87.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-87 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-88.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-88 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-89.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-89 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-90.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-90 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-91.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-91 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-92.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-92 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-93.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-93 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-94.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-94 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-95.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-95 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-96.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-96 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-97.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-97 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-98.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-98 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-99.                                                    
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-99 " TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-100.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-100" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-101.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-101" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-102.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-102" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-103.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-103" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-104.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-104" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-105.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-105" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-106.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-106" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-107.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-107" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-108.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-108" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-109.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-109" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-110.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-110" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-111.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-111" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-112.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-112" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-113.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-113" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-114.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-114" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-115.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-115" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-116.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-116" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-117.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-117" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-118.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-118" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-119.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-119" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-120.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-120" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-121.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-121" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-122.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-122" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-123.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-123" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-124.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-124" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-125.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-125" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-126.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-126" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-127.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-127" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-128.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-128" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-129.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-129" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-130.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-130" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-131.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-131" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-132.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-132" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-133.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-133" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-134.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-134" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-135.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-135" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-136.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-136" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-137.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-137" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-138.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-138" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-139.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-139" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-140.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-140" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-141.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-141" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-142.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-142" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-143.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-143" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-144.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-144" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-145.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-145" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-146.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-146" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-147.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-147" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-148.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-148" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-149.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-149" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-150.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-150" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-151.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-151" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-152.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-152" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-153.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-153" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-154.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-154" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-155.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-155" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-156.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-156" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-157.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-157" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-158.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-158" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-159.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-159" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-160.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-160" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-161.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-161" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-162.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-162" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-163.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-163" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-164.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-164" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-165.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-165" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-166.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-166" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-167.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-167" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-168.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-168" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-169.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-169" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-170.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-170" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-171.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-171" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-172.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-172" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-173.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-173" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-174.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-174" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-175.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-175" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-176.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-176" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-177.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-177" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-178.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-178" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-179.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-179" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-180.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-180" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-181.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-181" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-182.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-182" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-183.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-183" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-184.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-184" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-185.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-185" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-186.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-186" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-187.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-187" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-188.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-188" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-189.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-189" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-190.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-190" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-191.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-191" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-192.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-192" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-193.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-193" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-194.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-194" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-195.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-195" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-196.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-196" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-197.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-197" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-198.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-198" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-199.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-199" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        COPY-TEST-200.                                                   
                            MOVE     "1600-CARD LIBRARY" TO FEATURE.                     
                            MOVE     "COPY-TEST-200" TO PAR-NAME.                        
                            PERFORM  PASS.                                               
                            MOVE     SPACE TO COMPUTED-A.                                
                            MOVE     SPACE TO CORRECT-A.                                 
                            MOVE     SPACE TO RE-MARK.                                   
                            PERFORM  PRINT-DETAIL.                                       
                        CCVS-EXIT SECTION.                                               
                        CCVS-999999.                                                     
                            GO TO CLOSE-FILES.                                           
                    """.trimIndent()
                )
            }
        }
    )

    @Test
    fun sm207A() = rewriteRun(
        cobolCopy(getNistSource("SM207A.CBL")) { spec ->
            spec.afterRecipe { cu ->
                val outputCapture = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                printer.visit(cu, outputCapture)
                val result = outputCapture.getOut().trimIndent()
                assertThat(result).isEqualTo(
                    """
                        IDENTIFICATION DIVISION.                                         
                        PROGRAM-ID.                                                      
                            SM207A.                                                      
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
                            "SM207A".                                                    
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
                        SECT-SM207A-001 SECTION.                                         
                        QUAL-TEST-01.                                                    
                            MOVE "NOTHING COPIED" TO RE-MARK.                            
                            PERFORM FAIL.                                                
                            MOVE SPACES TO RE-MARK.                                      
                            PERFORM PASS.                                                
                            SUBTRACT 1 FROM ERROR-COUNTER.                               
                            GO TO QUAL-WRITE-01.                                         
                        QUAL-DELETE-01.                                                  
                            PERFORM DE-LETE.                                             
                        QUAL-WRITE-01.                                                   
                            MOVE "QUAL-TEST-01" TO PAR-NAME.                             
                            MOVE "QUALIFIED LIBRY NAME" TO FEATURE.                      
                            PERFORM PRINT-DETAIL.                                        
                        QUAL-TEST-02.                                                    
                            ADD 1 TO ERROR-COUNTER.                                      
                            MOVE SPACES TO RE-MARK.                                      
                            PERFORM PASS.                                                
                            SUBTRACT 1 FROM ERROR-COUNTER.                               
                            IF P-OR-F IS EQUAL TO "PASS "                                
                                PERFORM FAIL                                             
                                MOVE "TEXT COPIED FROM WRONG LIBRARY" TO RE-MARK         
                                GO TO QUAL-WRITE-02.                                     
                            IF P-OR-F IS EQUAL TO "FAIL*"                                
                                PERFORM PASS                                             
                                SUBTRACT 1 FROM ERROR-COUNTER                            
                                MOVE SPACES TO RE-MARK                                   
                                GO TO QUAL-WRITE-02.                                     
                            PERFORM FAIL.                                                
                            SUBTRACT 1 FROM ERROR-COUNTER.                               
                            MOVE "NOTHING COPIED" TO RE-MARK.                            
                            GO TO QUAL-WRITE-02.                                         
                        QUAL-DELETE-02.                                                  
                            PERFORM DE-LETE.                                             
                        QUAL-WRITE-02.                                                   
                            MOVE "QUAL-TEST-02" TO PAR-NAME.                             
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
    fun sm301M() = rewriteRun(
        cobolCopy(getNistSource("SM301M.CBL")) { spec ->
            spec.afterRecipe { cu ->
                val outputCapture = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                printer.visit(cu, outputCapture)
                val result = outputCapture.getOut().trimIndent()
                assertThat(result).isEqualTo(
                    """
                        IDENTIFICATION DIVISION.                                         
                        PROGRAM-ID.                                                      
                             SM301M.                                                     
                        ENVIRONMENT DIVISION.                                            
                        CONFIGURATION SECTION.                                           
                        SOURCE-COMPUTER.                                                 
                            XXXXX082.                                                    
                        OBJECT-COMPUTER.                                                 
                            XXXXX083.                                                    
                        DATA DIVISION.                                                   
                        PROCEDURE DIVISION.                                              
                        SM301M-CONTROL.                                                  
                            PERFORM SM301M-COPY.                                         
                            STOP RUN.                                                    
                        SM301M-COPY.                                                     
                            DISPLAY " ".                                                 
                    """.trimIndent()
                )
            }
        }
    )

    @Test
    fun sm401M() = rewriteRun(
        cobolCopy(getNistSource("SM401M.CBL")) { spec ->
            spec.afterRecipe { cu ->
                val outputCapture = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                printer.visit(cu, outputCapture)
                val result = outputCapture.getOut().trimIndent()
                assertThat(result).isEqualTo(
                    """
                        IDENTIFICATION DIVISION.                                         
                        PROGRAM-ID.                                                      
                            SM401M.                                                      
                        ENVIRONMENT DIVISION.                                            
                        CONFIGURATION SECTION.                                           
                        SOURCE-COMPUTER.                                                 
                            XXXXX082.                                                    
                        OBJECT-COMPUTER.                                                 
                            XXXXX083.                                                    
                        DATA DIVISION.                                                   
                        PROCEDURE DIVISION.                                              
                        SM401M-CONTROL.                                                  
                            PERFORM SM401M-COPYREP THRU SM401M-REPL.                     
                            STOP RUN.                                                    
                        SM401M-COPYREP.                                                  
                            DISPLAY "COW SHEEP PIG HORSE LAMB DOG CAT ".                 
                        SM401M-REPL.                                                     
                    """.trimIndent()
                )
            }
        }
    )
}
