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
import org.openrewrite.cobol.Assertions.cobolPreprocess
import org.openrewrite.cobol.Assertions.cobolPreprocessorCopy
import org.openrewrite.cobol.CobolPreprocessorVisitor
import org.openrewrite.cobol.internal.CobolPreprocessorOutputPrinter
import org.openrewrite.cobol.internal.IbmAnsi85
import org.openrewrite.internal.EncodingDetectingInputStream
import org.openrewrite.test.RecipeSpec
import org.openrewrite.test.RewriteTest
import org.openrewrite.test.RewriteTest.toRecipe
import java.nio.file.Files
import java.nio.file.Paths

class CobolPreprocessorReplaceTest : RewriteTest {

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
        spec.recipe(toRecipe {
            object : CobolPreprocessorVisitor<ExecutionContext>() {
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

    // TODO: add validation for replaced COPY BOOKS.
    @Test
    fun sm201A() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("SM201A.CBL"))
    )

    @Test
    fun sm202A() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("SM202A.CBL"))
    )

    @Test
    fun sm203A() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("SM203A.CBL"))
    )

    @Test
    fun sm205A() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("SM205A.CBL"))
    )

    @Test
    fun sm206A() = rewriteRun(
        cobolPreprocessorCopy(
            getNistSource("SM206A.CBL")) { spec ->
                spec.afterRecipe { cu ->
                    val statements = cu.cobols.filter { it is CobolPreprocessor.CopyStatement }
                    assertThat(statements.size).isEqualTo(9)

                    var output = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                    val printer = CobolPreprocessorOutputPrinter<ExecutionContext>(dialect, false)
                    printer.visit(statements[0], output)
                    assertThat(output.getOut()).isEqualTo("""
                        PST-TEST-001.                                                    
                            MOVE    "PSEUDO-TEXT" TO FEATURE.                            
                            MOVE    "PST-TEST-001" TO PAR-NAME                           
                            PERFORM PASS.                                                
                                                                             
                        PST-WRITE-001.                                                   
                            PERFORM PRINT-DETAIL.                                        
                    """.trimIndent())

                    output = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                    printer.visit(statements[1], output)
                    assertThat(output.getOut().trimIndent()).isEqualTo("""
                        MOVE   +00009 TO WRK-DS-05V00-O005-001  IN WRK-XN-00050-O005F-001 OF GRP-006 OF GRP-004 IN GRP-003 ( 2 ).         
                        ADD                                                          
                            +00001 TO                                                
                                      WRK-DS-05V00-O005-001                               
                                                      IN                       
                                                               WRK-XN-00050-O005F-001                     
                                                                IN                 
                                     GRP-006 IN GRP-004 IN GRP-002 IN GRP-001 ( 1 ).           
                    """.trimIndent())

                    output = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                    printer.visit(statements[2], output)
                    assertThat(output.getOut().trimIndent()).isEqualTo("""
                        MOVE   +00009 TO WRK-DS-05V00-O005-001  IN WRK-XN-00050-O005F-001 OF GRP-006 OF GRP-004 IN GRP-003 ( 2 ).         
                        ADD                                                          
                            +00001 TO                                                
                                      WRK-DS-05V00-O005-001                               
                                                      IN                       
                                                               WRK-XN-00050-O005F-001                     
                                                                IN                 
                                     GRP-006 IN GRP-004 IN GRP-002 IN GRP-001 ( 1 ).           
                    """.trimIndent())
                    println()
                }
            }
    )

    @Test
    fun sm208A() = rewriteRun(
        cobolPreprocess(getNistSource("SM208A.CBL")) { spec ->
            spec.afterRecipe {
                object : CobolPreprocessorVisitor<ExecutionContext>() {
                    override fun visitCopyBook(
                        copyBook: CobolPreprocessor.CopyBook,
                        p: ExecutionContext
                    ): CobolPreprocessor {
                        // Assert the replacement statement has been applied to the copied source.
                        val word = (copyBook.ast as CobolPreprocessor.CharDataLine).words[1] as CobolPreprocessor.Word
                        assertThat(word.word).isEqualTo("\"PASS\"")
                        return copyBook
                    }
                }
            }
        }
    )
}
