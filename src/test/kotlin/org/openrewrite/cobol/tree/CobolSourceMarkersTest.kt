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

class CobolSourceMarkersTest : RewriteTest {

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
    fun current() = rewriteRun(
        cobol("""
      *HEADER,COBOL,NC302M                                                            
000100 IDENTIFICATION DIVISION.                                         NC3024.2
000200 PROGRAM-ID.                                                      NC3024.2
000300     NC302M.                                                      NC3024.2
000400*THE FOLLOWING PROGRAM TESTS THE FLAGGING OF OBSOLETE             NC3024.2
000500*MINIMUM SUBSET NUCLEUS FEATURES.                                 NC3024.2
000600 AUTHOR. DAVID G BAMBER.                                          NC3024.2
000700*Message expected for above statement: OBSOLETE                   NC3024.2
000800 INSTALLATION. NCC.                                               NC3024.2
000900*Message expected for above statement: OBSOLETE                   NC3024.2
001000 DATE-WRITTEN. 19TH AUG 1988.                                     NC3024.2
001100*Message expected for above statement: OBSOLETE                   NC3024.2
001200 SECURITY. NO SECURITY.                                           NC3024.2
001300*Message expected for above statement: OBSOLETE                   NC3024.2
001400 ENVIRONMENT DIVISION.                                            NC3024.2
001500 CONFIGURATION SECTION.                                           NC3024.2
001600 SOURCE-COMPUTER.                                                 NC3024.2
001700     XXXXX082.                                                    NC3024.2
001800 OBJECT-COMPUTER.                                                 NC3024.2
001900     XXXXX083                                                     NC3024.2
002000     MEMORY SIZE                                                  NC3024.2
002100     XXXXX068                                                     NC3024.2
002200     CHARACTERS.                                                  NC3024.2
002300*Message expected for above statement: OBSOLETE                   NC3024.2
002400                                                                  NC3024.2
002500                                                                  NC3024.2
002600 DATA DIVISION.                                                   NC3024.2
002700 PROCEDURE DIVISION.                                              NC3024.2
002800                                                                  NC3024.2
002900 NC302M-CONTROL.                                                  NC3024.2
003000     PERFORM NC302M-ALTER THRU NC302M-STOP.                       NC3024.2
003100     STOP RUN.                                                    NC3024.2
003200                                                                  NC3024.2
003300 NC302M-ALTER.                                                    NC3024.2
003400     ALTER NC302M-PROC1 TO NC302M-PROC2.                          NC3024.2
003500*Message expected for above statement: OBSOLETE                   NC3024.2
003600                                                                  NC3024.2
003700 NC302M-PROC1.                                                    NC3024.2
003800     GO TO NC302M-PROC2.                                          NC3024.2
003900                                                                  NC3024.2
004000 NC302M-PROC2.                                                    NC3024.2
004100     DISPLAY "DUMMY PROCEDURE".                                   NC3024.2
004200                                                                  NC3024.2
004300                                                                  NC3024.2
004400                                                                  NC3024.2
004500                                                                  NC3024.2
004600 NC302M-STOP.                                                     NC3024.2
004700     STOP "FNC302".                                               NC3024.2
004800*Message expected for above statement: OBSOLETE                   NC3024.2
004900                                                                  NC3024.2
005000                                                                  NC3024.2
005100*TOTAL NUMBER OF FLAGS EXPECTED = 7.                              NC3024.2
      *END-OF,NC302M                                                                  
        """)
    )

    @Test
    fun lineNumbers() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION DIVISION.                                         C_AREA.1
            000002 PROGRAM-ID. communicationSection.                                C_AREA.2
        """)
    )

    @Test
    fun dotSeparators() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION DIVISION                                          C_AREA.1
            000002    .                                                             C_AREA.2
            000003 PROGRAM-ID                                                       C_AREA.3
            000004     .        communicationSection   .                            C_AREA.4
        """)
    )

    @Test
    fun singleLineStringLiteral() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION DIVISION.                                         
            000002 PROGRAM-ID. communicationSection.                                
            000003 PROCEDURE DIVISION.                                              
            000004     IF  SOME-DAT                                                 
            000005         DISPLAY '-------------------------------------------'    
            000006     END-IF.                                                      
            000007 EXIT.                                                            
        """)
    )

    @Test
    fun continuationLiteral() = rewriteRun(
        cobol("""
           000001  IDENTIFICATION DIVISION.                                        
           000002 PROGRAM-ID. communicationSection.                                
           000003 PROCEDURE DIVISION.                                              
           000004 DISPLAY '--------------------------------------------------------
           000005-    'on another line'                                            
           000006 EXIT.                                                            
        """)
    )

    @Test
    fun multipleContinuationLiteral() = rewriteRun(
        cobol("""
           000001 IDENTIFICATION DIVISION.                                         C_AREA.1
           000002 PROGRAM-ID. communicationSection.                                C_AREA.2
           000003 PROCEDURE DIVISION.                                              C_AREA.3
           000004     IF  SOME-DAT                                                 C_AREA.4
           000005         DISPLAY 'first line                                      C_AREA.5
           000006-    ' second line                                                C_AREA.6
           000007-    ' third line'                                                C_AREA.7
           000008     END-IF.                                                      C_AREA.8
           000009 EXIT.                                                            C_AREA.9
        """)
    )

    @Test
    fun multipleContinuationLiteralNoCommentArea() = rewriteRun(
        cobol("""
           000001 IDENTIFICATION DIVISION.                                         
           000002 PROGRAM-ID. communicationSection.                                
           000003 PROCEDURE DIVISION.                                              
           000004     IF  SOME-DAT                                                 
           000005         DISPLAY 'first line                                      
           000006-    'second line                                                 
           000007-    'third line'                                                 
           000008     END-IF.                                                      
           000009 EXIT.                                                            
        """)
    )

    @Test
    fun continuationWithoutNewLine() = rewriteRun(
        cobol("""
           000001 IDENTIFICATION DIVISION.                                         
           000002 PROGRAM-ID. communicationSection.                                
           000003 PROCEDURE DIVISION.                                              
           000004    DISPLAY 'first line                                           
           000005-    'second line'    .                                           
        """)
    )

    @Test
    fun emptyContinuation() = rewriteRun(
        cobol("""
           000001 IDENTIFICATION DIVISION.                                         
           000002 PROGRAM-ID. communicationSection.                                
           000003 PROCEDURE DIVISION.                                              
           000004    DISPLAY 'Because it will happen                               
           000005-    ''    .                                                      
        """)
    )

    @Test
    fun literalStartsOnNewLine() = rewriteRun(
        cobol("""
           000001  IDENTIFICATION DIVISION.                                        
           000002 PROGRAM-ID. communicationSection.                                
           000003 PROCEDURE DIVISION.                                              
           000004 DISPLAY                                                          
           000005 '----------------------------------------------------------------
           000006-    'on another line'                                            
           000007 EXIT.                                                            
        """)
    )

    @Disabled("Fix: Comma delimiters are pipped to a hidden channel.")
    @Test
    fun commaDelimiter() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION DIVISION.                                         C_AREA.01
            000002 PROGRAM-ID. acceptStatement.                                     C_AREA.02
            000003 PROCEDURE DIVISION.                                              C_AREA.03
            000004 COMMA-SUBSCRIPT-TEST.                                            C_AREA.04
            000005 EVALUATE SUBSCRIPT                                               C_AREA.05
            000006 WHEN IDENTIFIER (FUNCTION INTEGER (IDENTIFIER                    C_AREA.06
            000007 , IDENTIFIER IDENTIFIER) (1: 10))                                C_AREA.07
            000008     CONTINUE.                                                    C_AREA.08
        """)
    )

    // CRLF
    @Test
    fun continuationWithCRLF() = rewriteRun(
        cobol("" +
           "000001 IDENTIFICATION DIVISION.                                         \r\n" +
           "000002 PROGRAM-ID. communicationSection.                                \r\n" +
           "000003 PROCEDURE DIVISION.                                              \r\n" +
           "000004    DISPLAY 'first line                                           \r\n" +
           "000005-    ' second line'    .                                          \r\n"
        )
    )

    @Test
    fun commentAreaWithCRLF() = rewriteRun(
        cobol("" +
                "000001 IDENTIFICATION DIVISION.                                         C_AREA.1\r\n" +
                "000002 PROGRAM-ID. communicationSection.                                C_AREA.2\r\n"
        )
    )
}
