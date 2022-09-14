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
import org.openrewrite.cobol.Assertions.cobolCopy
import org.openrewrite.cobol.CobolPreprocessorParser
import org.openrewrite.cobol.CobolPreprocessorVisitor
import org.openrewrite.cobol.PreprocessReplaceVisitor
import org.openrewrite.internal.EncodingDetectingInputStream
import org.openrewrite.test.RecipeSpec
import org.openrewrite.test.RewriteTest
import org.openrewrite.test.RewriteTest.toRecipe
import java.nio.file.Files
import java.nio.file.Paths

class CobolPreprocessorReplaceTest : RewriteTest {

    companion object {
        // Paths.get(System.getProperty("user.home"), "rewrite-offline").toAbsolutePath();
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
            PreprocessReplaceVisitor()
        }.doNext(toRecipe {
            object : CobolPreprocessorVisitor<ExecutionContext>() {
                override fun visitSpace(space: Space, p: ExecutionContext): Space {
                    val whitespace = space.whitespace.trim()
                    // TODO: separators should be isolated to a dialect.
                    if (!(whitespace.equals(",") || whitespace.equals(";") || whitespace.isEmpty())) {
                        return space.withWhitespace("(~~>${space.whitespace}<~~)")
                    }
                    return space
                }
            }
        })).parser(CobolPreprocessorParser.builder())
    }

    @Test
    fun sm208A() = rewriteRun(
        cobolCopy(
            getNistSource("SM208A.CBL"),
            // The ReplacementVisitor adds replacements, but preserves the original source.
            getNistSource("SM208A.CBL")
        )
        // TODO: Add mechanism to print the replaced text.
        /* Add validation of replacements on copy.
            spec.afterRecipe {
                object : CobolPreprocessorVisitor<ExecutionContext>() {
                    override fun visitCopyBook(
                        copyBook: CobolPreprocessor.CopyBook,
                        p: ExecutionContext
                    ): CobolPreprocessor {
                        val word = (copyBook.ast as CobolPreprocessor.CharDataLine).words[1] as CobolPreprocessor.Word
                        assertThat(word.word).isEqualTo("\"PASS\"")
                        return copyBook
                    }
                }
            }
         */
    )
}
