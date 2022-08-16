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

class CobolNistTest : RewriteTest {

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

    @Disabled
    @Test
    fun lineNumbersWithCommentArea() = rewriteRun(
        cobol("""
            000000 IDENTIFICATION DIVISION.                                         CM1014.2
            000000 PROGRAM-ID. communicationSection.                                CM1014.2
        """)
    )

    @Disabled
    @Test
    fun lineNumbers() = rewriteRun(
        cobol("""
            000000 IDENTIFICATION DIVISION.                                         CM1014.2
            000000 PROGRAM-ID. communicationSection.                                CM1014.2
        """)
    )

    @Test
    fun continuationLiteral() = rewriteRun(
        cobol("""
            IDENTIFICATION DIVISION.                                     
            PROGRAM-ID. communicationSection.                            
            PROCEDURE DIVISION.                                          
                IF  SOME-DAT                                             
                    DISPLAY '--------------------------------------------
           -    'on another line'
                END-IF.                                                  
            EXIT.                                                        
        """)
    )

    @Test
    fun multipleContinuationLiteral() = rewriteRun(
        cobol("""
            IDENTIFICATION DIVISION.                                     
            PROGRAM-ID. communicationSection.                            
            PROCEDURE DIVISION.                                          
                IF  SOME-DAT                                             
                    DISPLAY 'first line                                  
           -    ' second line
           -    ' third line'
                END-IF.                                                  
            EXIT.                                                        
        """)
    )
}
