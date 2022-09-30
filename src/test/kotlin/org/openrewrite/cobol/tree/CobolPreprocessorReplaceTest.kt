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

    @Test
    fun sm201A() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("SM201A.CBL")) { spec ->
            spec.afterRecipe { cu ->
                val statements = cu.cobols.filter { it is CobolPreprocessor.CopyStatement }
                assertThat(statements.size).isEqualTo(8)

                val k1fda = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                val printer = CobolPreprocessorOutputPrinter<ExecutionContext>(dialect, false)
                printer.visit(statements[0], k1fda)
                var result = k1fda.getOut().trimIndent()
                assertThat(result).isEqualTo(
                    """
                        LABEL RECORDS STANDARD                                       
                        VALUE OF                                                     
                        XXXXX074                                                     
                        IS                                                           
                        XXXXX075                                                     
                        DATA RECORD IS TST-TEST.                                     
                    """.trimIndent())

                val k101aFirst = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                printer.visit(statements[1], k101aFirst)
                result = k101aFirst.getOut().trimIndent()
                assertThat(result).isEqualTo(
                    """
                                .                                                    
                        02 TF-1 PICTURE 9(5).                                   
                        02 FILLER    PICTURE X(115).                                 
                    """.trimIndent())

                val k101aSecond = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                printer.visit(statements[2], k101aSecond)
                result = k101aSecond.getOut().trimIndent()
                assertThat(result).isEqualTo(
                    """
                                .                                                    
                        02 FILLER PICTURE X(115).                                   
                        02 TXT-FLD-1    PIC 9(5).                                 
                    """.trimIndent())

                val k1wkaFirst = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                printer.visit(statements[3], k1wkaFirst)
                result = k1wkaFirst.getOut().trim()
                assertThat(result).isEqualTo("02 WSTR999 PICTURE X(3) VALUE                              \"ABC\".")

                val k1wkaSecond = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                printer.visit(statements[4], k1wkaSecond)
                result = k1wkaSecond.getOut().trim()
                assertThat(result).isEqualTo("02 WSTR-2A PICTURE X(3) VALUE                              \"ABC\".")

                val k1wkbFirst = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                printer.visit(statements[5], k1wkbFirst)
                result = k1wkbFirst.getOut().trimIndent()
                assertThat(result).isEqualTo(
                    """
                        02 WSTR91  PICTURE XXX VALUE "ABC".                          
                        02 WSTR92  PICTURE XXX VALUE "DEF".                          
                        02 WSTR93  PICTURE XXX VALUE "GHI".                          
                    """.trimIndent())

                val k1wkbSecond = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                printer.visit(statements[6], k1wkbSecond)
                assertThat(k1wkbSecond.getOut().trimIndent()).isEqualTo(
                    """
                        02 WSTR4A  PICTURE XXX VALUE "ABC".                          
                        02 WSTR4B  PICTURE XXX VALUE "DEF".                          
                        02 WSTR4C  PICTURE XXX VALUE "GHI".                          
                    """.trimIndent())

                val k1prb = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                printer.visit(statements[7], k1prb)
                result = k1prb.getOut().trimIndent()
                assertThat(result).isEqualTo(
                    """
                        MOVE WSTR4B TO WSTR91.                                       
                        MOVE WSTR4B TO WSTR93.                                       
                        MOVE WSTR4A TO WSTR92.                                       
                    """.trimIndent())
            }
        }
    )

    @Test
    fun sm202A() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("SM202A.CBL")) { spec ->
            spec.afterRecipe { cu ->
                val statements = cu.cobols.filter { it is CobolPreprocessor.CopyStatement }
                assertThat(statements.size).isEqualTo(3)

                val k2sea = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                val printer = CobolPreprocessorOutputPrinter<ExecutionContext>(dialect, false)
                printer.visit(statements[0], k2sea)
                var result = k2sea.getOut().trimIndent()
                assertThat(result).isEqualTo(
                    """
                        PARA-1.                                                          
                            ALTER    PARA-2 TO PROCEED TO PARA-4.                        
                        PARA-2.                                                          
                            GO       TO PARA-3.                                           
                        PARA-3.                                                           
                            PERFORM  FAIL.                                               
                            GO       TO COPY-WRITE-15.                                   
                        PARA-4.                                                          
                            PERFORM  PASS.                                         
                            GO       TO COPY-WRITE-15.                                   
                    """.trimIndent())

                val k2praFirst = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                printer.visit(statements[1], k2praFirst)
                result = k2praFirst.getOut().trimIndent()
                assertThat(result).isEqualTo(
                    """
                        MOVE     "TRUE " TO AREA-1.                             
                        MOVE     " TWO${'$'}" TO AREA-2.                             
                        MOVE     "+ 2 =" TO AREA-3.                             
                        MOVE     4 TO AREA-4.                             
                        IF       TOTAL-AREA EQUAL TO "TRUE  TWO + 2 =    4"          
                                 PERFORM PASS ELSE PERFORM FAIL.                     
                        GO       TO COPY-WRITE-16.                                   
                    """.trimIndent())

                val k2praSecond = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                printer.visit(statements[2], k2praSecond)
                result = k2praSecond.getOut().trimIndent()
                assertThat(result).isEqualTo(
                    """
                        MOVE     TRUE-Q-04 OF TRUE-Q-03                                                 IN TRUE-Q-02 TO AREA-1.                             
                        MOVE     " TWO FIVE " TO AREA-2.                             
                        MOVE     Z (2, 1, 1) TO AREA-3.                             
                        MOVE     +000004.99 TO AREA-4.                             
                        IF       TOTAL-AREA EQUAL TO "TRUE  TWO + 2 =    4"          
                                 PERFORM PASS ELSE PERFORM FAIL.                     
                        GO       TO COPY-WRITE-17.                                   
                    """.trimIndent())
            }
        }
    )

    @Test
    fun sm203A() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("SM203A.CBL")) { spec ->
            spec.afterRecipe { cu ->
                val statements = cu.cobols.filter { it is CobolPreprocessor.CopyStatement }
                assertThat(statements.size).isEqualTo(3)

                val k3snb = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                val printer = CobolPreprocessorOutputPrinter<ExecutionContext>(dialect, false)
                printer.visit(statements[0], k3snb)
                var result = k3snb.getOut().trimIndent()
                assertThat(result).isEqualTo(
                    """
                        XXXXX051                                                     
                        IS SW-1                                                
                        ON STATUS IS SWITCH-ON                                        
                        OFF STATUS IS SWITCH-OFF.                                     
                    """.trimIndent())

                val k3fcb = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                printer.visit(statements[1], k3fcb)
                result = k3fcb.getOut().trimIndent()
                assertThat(result).isEqualTo(
                    """
                        SELECT PRINT-FILE ASSIGN TO                                  
                        XXXXX055.                                                    
                        SELECT TEST-FILE ASSIGN TO                             
                        XXXXP002.                                                    
                    """.trimIndent())

                val k3iob = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                printer.visit(statements[2], k3iob)
                result = k3iob.getOut().trim()
                assertThat(result).isEqualTo("SAME RECORD AREA FOR TEST-FILE, PRINT-FILE.")
            }
        }
    )

    @Test
    fun sm205A() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("SM205A.CBL")) { spec ->
            spec.afterRecipe { cu ->
                val statements = cu.cobols.filter { it is CobolPreprocessor.CopyStatement }
                assertThat(statements.size).isEqualTo(2)

                val k5sdb = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                val printer = CobolPreprocessorOutputPrinter<ExecutionContext>(dialect, false)
                printer.visit(statements[0], k5sdb)
                var result = k5sdb.getOut().trim()
                assertThat(result).isEqualTo("DATA RECORD S-RECORD.")

                val k501b = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                printer.visit(statements[1], k501b)
                result = k501b.getOut().trimIndent()
                assertThat(result).isEqualTo(
                    """
                        02  KEYS-GROUP.                                              
                            03  KEY-1 PICTURE 9.                                     
                            03  KEY-2 PICTURE 99.                                    
                            03  KEY-3 PICTURE 999.                                   
                            03  KEY-4 PICTURE 9999.                                  
                            03  KEY-5 PICTURE 99999.                                 
                        02 RDF-KEYS REDEFINES KEYS-GROUP PICTURE 9(15).              
                        02 FILLER PICTURE X(105).                                    
                    """.trimIndent())
            }
        }
    )

    @Test
    fun sm206A() = rewriteRun(
        cobolPreprocessorCopy(
            getNistSource("SM206A.CBL")) { spec ->
                spec.afterRecipe { cu ->
                    val statements = cu.cobols.filter { it is CobolPreprocessor.CopyStatement }
                    assertThat(statements.size).isEqualTo(9)

                    val kp001 = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                    val printer = CobolPreprocessorOutputPrinter<ExecutionContext>(dialect, false)
                    printer.visit(statements[0], kp001)
                    var result = kp001.getOut().trimIndent()
                    assertThat(result).isEqualTo(
                    """
                        PST-TEST-001.                                                    
                            MOVE    "PSEUDO-TEXT" TO FEATURE.                            
                            MOVE    "PST-TEST-001" TO PAR-NAME                           
                            PERFORM PASS.                                                
                                                                             
                        PST-WRITE-001.                                                   
                            PERFORM PRINT-DETAIL.                                        
                    """.trimIndent())

                    val kp002 = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                    printer.visit(statements[1], kp002)
                    result = kp002.getOut().trimIndent()
                    assertThat(result).isEqualTo(
                    """
                        MOVE   +00009 TO WRK-DS-05V00-O005-001  IN WRK-XN-00050-O005F-001 OF GRP-006 OF GRP-004 IN GRP-003 ( 2 ).         
                        ADD                                                          
                            +00001 TO                                                
                                      WRK-DS-05V00-O005-001                               
                                                      IN                       
                                                               WRK-XN-00050-O005F-001                     
                                                                IN                 
                                     GRP-006 IN GRP-004 IN GRP-002 IN GRP-001 (1).           
                    """.trimIndent())

                    val kp003 = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                    printer.visit(statements[2], kp003)
                    result = kp003.getOut().trimIndent()
                    assertThat(result).isEqualTo(
                    """
                        PST-TEST-003.                                                    
                            MOVE    +0009 TO WRK-DS-05V00-O005-001  IN GRP-003 (3).      
                            ADD     +2 TO WRK-DS-09V00-901.                          
                            SUBTRACT -3 FROM WRK-DS-05V00-O005-001 IN GRP-002 (3).        
                        PST-EXIT-003-X.                                                  
                    """.trimIndent())

                    val kp004 = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                    printer.visit(statements[3], kp004)
                    result = kp004.getOut().trimIndent()
                    assertThat(result).isEqualTo(
                    """
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
                            PERFORM DE-LETE.                                              
                        PST-EXIT-004.                                                    
                            EXIT.                                                        
                    """.trimIndent())

                    val kp005 = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                    printer.visit(statements[4], kp005)
                    result = kp005.getOut().trim()
                    assertThat(result).isEqualTo("MOVE 7 TO WRK-DS-09V00-901.")

                    val kp006 = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                    printer.visit(statements[5], kp006)
                    result = kp006.getOut().trim()
                    assertThat(result).isEqualTo("ADD      001005 TO WRK-DS-09V00-901.")

                    val kp007 = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                    printer.visit(statements[6], kp007)
                    assertThat(kp007.getOut().trim()).isEqualTo("PERFORM PASS.")

                    val kp008 = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                    printer.visit(statements[7], kp008)
                    result = kp008.getOut().trim()
                    assertThat(result).isEqualTo("PERFORM PASS.")

                    val kp009 = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                    printer.visit(statements[8], kp009)
                    result = kp009.getOut().trim()
                    assertThat(result).isEqualTo("IF      WRK-XN-00001 = \"T\"")
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
