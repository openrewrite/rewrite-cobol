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
import org.openrewrite.cobol.internal.CobolPreprocessorOutputPrinter
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
        cobolCopy(getNistSource("SM201A.CBL"))
    )

    @Test
    fun sm202A() = rewriteRun(
        cobolCopy(getNistSource("SM202A.CBL"))
    )

    @Test
    fun sm203A() = rewriteRun(
        cobolCopy(getNistSource("SM203A.CBL"))
    )

    @Test
    fun sm205A() = rewriteRun(
        cobolCopy(getNistSource("SM205A.CBL"))
    )

    @Test
    fun sm206A() = rewriteRun(
        cobolCopy(getNistSource("SM206A.CBL"))
    )

    @Test
    fun sm208A() = rewriteRun(
        cobolCopy(getNistSource("SM208A.CBL"))
    )
}
