/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.cobol.tree.preprocessor

import org.assertj.core.api.Assertions.assertThat
import org.junit.jupiter.api.Test
import org.openrewrite.ExecutionContext
import org.openrewrite.InMemoryExecutionContext
import org.openrewrite.PrintOutputCapture
import org.openrewrite.cobol.CobolPreprocessorVisitor
import org.openrewrite.cobol.internal.CobolDialect
import org.openrewrite.cobol.internal.CobolPreprocessorOutputSourcePrinter
import org.openrewrite.cobol.internal.CobolPreprocessorPrinter
import org.openrewrite.cobol.tree.CobolPreprocessor
import org.openrewrite.cobol.tree.CobolTest
import org.openrewrite.cobol.tree.PreprocessorParserAssertions.cobolPreprocessorCopy
import org.openrewrite.cobol.tree.Space
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
                    assertThat(copyStatement.copyBook).isNotNull
                    assertThat(copyStatement.copyBook!!.sourcePath).isNotNull
                    val copyBook = copyStatement.copyBook

                    val output = PrintOutputCapture<ExecutionContext>(InMemoryExecutionContext())
                    val printer =
                        CobolPreprocessorOutputSourcePrinter<ExecutionContext>(
                            CobolDialect.ibmAnsi85(),
                            true
                        )
                    printer.visit(copyBook, output)

                    val source = getSource(copyBook!!.sourcePath)
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
