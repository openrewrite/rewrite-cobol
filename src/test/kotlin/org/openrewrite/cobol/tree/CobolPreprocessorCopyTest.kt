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
import org.openrewrite.cobol.CobolPreprocessorParser
import org.openrewrite.cobol.CobolPreprocessorVisitor
import org.openrewrite.cobol.internal.CobolPreprocessorOutputPrinter
import org.openrewrite.cobol.internal.IbmAnsi85
import org.openrewrite.internal.EncodingDetectingInputStream
import org.openrewrite.test.RecipeSpec
import org.openrewrite.test.RewriteTest
import org.openrewrite.test.RewriteTest.toRecipe
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

class CobolPreprocessorCopyTest : RewriteTest {

    companion object {
        val dialect = IbmAnsi85()

        private val userDir = System.getProperty("user.dir")
        private const val nistPath = "/src/test/resources/gov/nist/"
        fun getNistSource(sourceName: String): String {
            val path = Paths.get(userDir + nistPath + sourceName)
            val inputStream = Files.newInputStream(path)
            val encoding = EncodingDetectingInputStream(inputStream)
            return encoding.readFully()
        }
    }

    override fun defaults(spec: RecipeSpec) {
        spec.parser(CobolPreprocessorParser.builder().setEnableCopy(true))
            .recipe(toRecipe {
            object : CobolPreprocessorVisitor<ExecutionContext>() {
                override fun visitSpace(space: Space, p: ExecutionContext): Space {
                    val whitespace = space.whitespace.trim()
                    if (!(dialect.separators.contains("$whitespace ") || whitespace.isEmpty())) {
                        return space.withWhitespace("(~~>${space.whitespace}<~~)")
                    }
                    return space
                }

                override fun visitCopyStatement(
                    copyStatement: CobolPreprocessor.CopyStatement,
                    p: ExecutionContext
                ): CobolPreprocessor {
                    val copyBook = copyStatement.copyBook

                    val output = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                    val printer = CobolPreprocessorOutputPrinter<ExecutionContext>(IbmAnsi85(), true)
                    printer.visit(copyBook, output)

                    val source = getSource(copyBook!!.sourcePath)
                    assertThat(source).isEqualTo(output.getOut())
                    return super.visitCopyStatement(copyStatement, p)
                }
            }
        }).parser(CobolPreprocessorParser.builder())
    }

    private fun getSource(copyBook: Path): String {
        var source: String
        try {
            Files.newInputStream(copyBook).use { inputStream ->
                val input = EncodingDetectingInputStream(inputStream)
                source = input.readFully()
            }
        } catch (e: Exception) {
            throw RuntimeException(e)
        }
        return source
    }

    @Test
    fun sm101A() = rewriteRun(
        cobolCopy(getNistSource("SM101A.CBL"))
    )

    @Test
    fun sm103A() = rewriteRun(
        cobolCopy(getNistSource("SM103A.CBL"))
    )

    @Test
    fun sm105A() = rewriteRun(
        cobolCopy(getNistSource("SM105A.CBL"))
    )

    @Test
    fun sm106A() = rewriteRun(
        cobolCopy(getNistSource("SM106A.CBL"))
    )

    @Test
    fun sm107A() = rewriteRun(
        cobolCopy(getNistSource("SM107A.CBL"))
    )

    @Test
    fun sm207A() = rewriteRun(
        cobolCopy(getNistSource("SM207A.CBL"))
    )

    @Test
    fun sm301M() = rewriteRun(
        cobolCopy(getNistSource("SM301M.CBL"))
    )

    @Test
    fun sm401M() = rewriteRun(
        cobolCopy(getNistSource("SM401M.CBL"))
    )
}
