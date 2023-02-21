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
import org.openrewrite.cobol.tree.ParserAssertions.cobolPreprocessorCopy
import org.openrewrite.cobol.CobolPreprocessorVisitor
import org.openrewrite.cobol.internal.CobolDialect
import org.openrewrite.cobol.internal.CobolPreprocessorOutputSourcePrinter
import org.openrewrite.cobol.internal.CobolPreprocessorPrinter
import org.openrewrite.internal.EncodingDetectingInputStream
import org.openrewrite.test.RecipeSpec
import org.openrewrite.test.RewriteTest.toRecipe
import java.nio.file.Files
import java.nio.file.Path

class CobolPreprocessorCopyTest : CobolTest() {

    companion object {
        val printer = CobolPreprocessorPrinter<ExecutionContext>(false, true)
    }

    override fun defaults(spec: RecipeSpec) {
        spec.recipe(toRecipe { _ ->
            object : CobolPreprocessorVisitor<ExecutionContext>() {
                override fun visitSpace(space: Space, location: Space.Location, p: ExecutionContext): Space {
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
                    val printer =
                        CobolPreprocessorOutputSourcePrinter<ExecutionContext>(
                            CobolDialect.ibmAnsi85(),
                            true
                        )
                    printer.visit(copyBook, output)

                    val source = getSource(copyBook.sourcePath)
                    assertThat(source).isEqualTo(output.getOut())
                    return super.visitCopyStatement(copyStatement, p)
                }
            }
        })
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
        cobolPreprocessorCopy(getNistSource("SM101A.CBL"))
    )

    @Test
    fun sm103A() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("SM103A.CBL"))
    )

    @Test
    fun sm105A() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("SM105A.CBL"))
    )

    @Test
    fun sm106A() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("SM106A.CBL"))
    )

    @Test
    fun sm107A() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("SM107A.CBL"))
    )

    @Test
    fun sm207A() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("SM207A.CBL"))
    )

    @Test
    fun sm301M() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("SM301M.CBL"))
    )

    @Test
    fun sm401M() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("SM401M.CBL"))
    )
}
