/*
 * Copyright 2023 the original author or authors.
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
package org.openrewrite.cobol.tree.preprocessor

import org.junit.jupiter.api.Test
import org.openrewrite.ExecutionContext
import org.openrewrite.cobol.CobolPreprocessorVisitor
import org.openrewrite.cobol.internal.CobolPreprocessorPrinter
import org.openrewrite.cobol.tree.CobolTest
import org.openrewrite.cobol.tree.PreprocessorParserAssertions.cobolPreprocessorCopy
import org.openrewrite.cobol.tree.Space
import org.openrewrite.test.RecipeSpec
import org.openrewrite.test.RewriteTest.toRecipe

class CobolPreprocessorCopyBookTest : CobolTest() {

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
            }
        })
    }

    @Test
    fun altl1() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("ALTL1.CPY"))
    )

    @Test
    fun altlb() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("ALTLB.CPY"))
    )

    @Test
    fun k1daa() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K1DAA.CPY"))
    )

    @Test
    fun k1fda() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K1FDA.CPY"))
    )

    @Test
    fun k1p01() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K1P01.CPY"))
    )

    @Test
    fun k1pra() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K1PRA.CPY"))
    )

    @Test
    fun k1prb() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K1PRB.CPY"))
    )

    @Test
    fun k1prc() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K1PRC.CPY"))
    )

    @Test
    fun k1sea() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K1SEA.CPY"))
    )

    @Test
    fun k1w01() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K1W01.CPY"))
    )

    @Test
    fun k1w02() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K1W02.CPY"))
    )

    @Test
    fun k1w03() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K1W03.CPY"))
    )

    @Test
    fun k1w04() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K1W04.CPY"))
    )

    @Test
    fun k1wka() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K1WKA.CPY"))
    )

    @Test
    fun k1wkb() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K1WKB.CPY"))
    )

    @Test
    fun k1wkc() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K1WKC.CPY"))
    )

    @Test
    fun k1wky() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K1WKY.CPY"))
    )

    @Test
    fun k1wkz() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K1WKZ.CPY"))
    )

    @Test
    fun k2pra() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K2PRA.CPY"))
    )

    @Test
    fun k2sea() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K2SEA.CPY"))
    )

    @Test
    fun k3fca() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K3FCA.CPY"))
    )

    @Test
    fun k3fcb() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K3FCB.CPY"))
    )

    @Test
    fun k3ioa() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K3IOA.CPY"))
    )

    @Test
    fun k3iob() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K3IOB.CPY"))
    )

    @Test
    fun k3lge() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K3LGE.CPY"))
    )

    @Test
    fun k3oca() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K3OCA.CPY"))
    )

    @Test
    fun k3sca() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K3SCA.CPY"))
    )

    @Test
    fun k3sml() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K3SML.CPY"))
    )

    @Test
    fun k3sna() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K3SNA.CPY"))
    )

    @Test
    fun k3snb() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K3SNB.CPY"))
    )

    @Test
    fun k4nta() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K4NTA.CPY"))
    )

    @Test
    fun k5sda() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K5SDA.CPY"))
    )

    @Test
    fun k6sca() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K6SCA.CPY"))
    )

    @Test
    fun k7sea() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K7SEA.CPY"))
    )

    @Test
    fun k101A() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K101A.CPY"))
    )

    @Test
    fun k501A() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K501A.CPY"))
    )

    @Test
    fun k501B() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K501B.CPY"))
    )

    @Test
    fun kk208A() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("KK208A.CPY"))
    )

    @Test
    fun kp001() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("KP001.CPY"))
    )

    @Test
    fun kp002() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("KP002.CPY"))
    )

    @Test
    fun kp003() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("KP003.CPY"))
    )

    @Test
    fun kp004() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("KP004.CPY"))
    )

    @Test
    fun kp005() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("KP005.CPY"))
    )

    @Test
    fun kp006() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("KP006.CPY"))
    )

    @Test
    fun kp007() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("KP007.CPY"))
    )

    @Test
    fun kp008() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("KP008.CPY"))
    )

    @Test
    fun kp009() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("KP009.CPY"))
    )

    @Test
    fun kp010() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("KP010.CPY"))
    )

    @Test
    fun ksm31() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("KSM31.CPY"))
    )

    @Test
    fun ksm41() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("KSM41.CPY"))
    )

    @Test
    fun trailingSub() = rewriteRun(
        cobolPreprocessorCopy(getNistSource("K1WKA_TRAILING_SUB.CPY"))
    )
}
