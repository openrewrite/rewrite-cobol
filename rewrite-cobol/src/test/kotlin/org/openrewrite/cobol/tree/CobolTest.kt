package org.openrewrite.cobol.tree

import io.github.classgraph.ClassGraph
import org.openrewrite.ExecutionContext
import org.openrewrite.cobol.CobolIsoVisitor
import org.openrewrite.cobol.internal.CobolDialect
import org.openrewrite.internal.StringUtils
import org.openrewrite.test.RecipeSpec
import org.openrewrite.test.RewriteTest

open class CobolTest : RewriteTest {

    companion object {
        val dialect: CobolDialect = CobolDialect.ibmAnsi85()
        val nistResourcePaths: List<String> by lazy {
            ClassGraph().acceptPaths("/gov/nist").scan()
                .allResources
                .paths
        }
    }

    override fun defaults(spec: RecipeSpec) {
        spec.recipe(RewriteTest.toRecipe { _ ->
            object : CobolIsoVisitor<ExecutionContext>() {
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

    fun getNistSource(sourceName: String): String {
        val source = nistResourcePaths.first { it.endsWith(sourceName) }
        return StringUtils.readFully(javaClass.getResourceAsStream("/$source")!!)
    }
}
