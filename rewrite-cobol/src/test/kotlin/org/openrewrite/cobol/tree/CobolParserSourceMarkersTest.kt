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
import org.openrewrite.cobol.Assertions.cobol

class CobolParserSourceMarkersTest : CobolTest() {

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

    @Test
    fun trailingComment() = rewriteRun(
        cobol(
            """
            000001 IDENTIFICATION DIVISION.                                         C_AREA.01
            000002 PROGRAM-ID. communicationSection.                                C_AREA.02
            000003* Trailing comment                                                C_AREA.02
        """
        )
    )

    @Test
    fun trailingWhitespace() = rewriteRun(
        cobol(
            """
            000001 IDENTIFICATION DIVISION.                                         C_AREA.01
            000002 PROGRAM-ID. communicationSection.                                C_AREA.02
            000003                                                                  C_AREA.02
        """
        )
    )
}
