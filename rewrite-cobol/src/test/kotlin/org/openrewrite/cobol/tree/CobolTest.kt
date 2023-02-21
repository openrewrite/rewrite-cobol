package org.openrewrite.cobol.tree

import io.github.classgraph.ClassGraph
import org.openrewrite.cobol.internal.CobolDialect
import org.openrewrite.internal.StringUtils
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

    fun getNistSource(sourceName: String): String {
        val source = nistResourcePaths.first { it.endsWith(sourceName) }
        return StringUtils.readFully(javaClass.getResourceAsStream("/$source")!!)
    }
}
