package org.openrewrite.cobol.search

import org.junit.jupiter.api.Test
import org.openrewrite.cobol.Assertions
import org.openrewrite.cobol.tree.CobolTest
import org.openrewrite.test.RecipeSpec

class FindIndicatorsTest : CobolTest() {

    override fun defaults(spec: RecipeSpec) {
        spec.recipe(FindIndicators(listOf("D", "d")))
    }

    @Test
    fun db101A() = rewriteRun(
        Assertions.cobol(getNistSource("DB101A.CBL"))
    )
}