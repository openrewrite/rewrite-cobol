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

import io.github.classgraph.ClassGraph
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.MethodSource
import org.openrewrite.cobol.Assertions.cobol
import org.openrewrite.internal.StringUtils
import org.openrewrite.test.RewriteTest
import java.nio.file.Path
import java.nio.file.Paths
import java.util.stream.Stream

class CobolCompatibilityTest : RewriteTest {
    @ParameterizedTest
    @MethodSource
    fun nist(resourcePath: Path) = rewriteRun(
        cobol(StringUtils.readFully(javaClass.getResourceAsStream("/$resourcePath")))
    )

    companion object {
        @JvmStatic
        fun nist(): Stream<Path>? {
            ClassGraph().acceptPaths("/gov/nist").scan().use { scanResult ->
                return scanResult.allResources.paths.stream().map(Paths::get)
            }
        }
    }
}
