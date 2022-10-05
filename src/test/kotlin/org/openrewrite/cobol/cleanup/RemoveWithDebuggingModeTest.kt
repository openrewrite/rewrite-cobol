package org.openrewrite.cobol.search

import org.junit.jupiter.api.Test
import org.openrewrite.cobol.Assertions.cobolCopy
import org.openrewrite.cobol.cleanup.RemoveWithDebuggingMode
import org.openrewrite.cobol.tree.CobolTest
import org.openrewrite.test.RecipeSpec

class RemoveWithDebuggingModeTest : CobolTest() {

    override fun defaults(spec: RecipeSpec) {
        spec.recipe(RemoveWithDebuggingMode())
    }

    @Test
    fun noChange() = rewriteRun(
        cobolCopy(getNistSource("CM101M.CBL"))
    )

    @Test
    fun db101a() = rewriteRun(
        cobolCopy(getNistSource("DB101A.CBL"))
    )

    @Test
    fun db102a() = rewriteRun(
        cobolCopy(getNistSource("DB102A.CBL"))
    )
}