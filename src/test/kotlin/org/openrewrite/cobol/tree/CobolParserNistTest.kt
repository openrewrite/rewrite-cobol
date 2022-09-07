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

import org.junit.jupiter.api.Disabled
import org.junit.jupiter.api.Test
import org.openrewrite.ExecutionContext
import org.openrewrite.cobol.Assertions.cobol
import org.openrewrite.cobol.CobolIbmAnsi85Parser
import org.openrewrite.cobol.CobolIsoVisitor
import org.openrewrite.internal.EncodingDetectingInputStream
import org.openrewrite.test.RecipeSpec
import org.openrewrite.test.RewriteTest
import java.nio.file.Files
import java.nio.file.Paths

class CobolParserNistTest : RewriteTest {

    companion object {
        private val userDir = System.getProperty("user.dir")
        private const val nistPath = "/src/test/resources/gov/nist/"
        fun getNistSource(sourceName: String): String {
            val path = Paths.get(userDir + nistPath + sourceName)
            val inputStream = Files.newInputStream(path)
            val encoding = EncodingDetectingInputStream(inputStream)
            return encoding.readFully()
        }
    }

    override fun defaults(spec: RecipeSpec) {
        spec.parser(CobolIbmAnsi85Parser.builder())
            .recipe(RewriteTest.toRecipe {
                object : CobolIsoVisitor<ExecutionContext>() {
                    override fun visitSpace(space: Space, p: ExecutionContext): Space {
                        val whitespace = space.whitespace.trim()
                        // TODO: separators should be isolated to a dialect.
                        if (!(whitespace.equals(",") || whitespace.equals(";") || whitespace.isEmpty())) {
                            return space.withWhitespace("(~~>${space.whitespace}<~~)")
                        }
                        return space
                    }
                }
            }).parser(CobolIbmAnsi85Parser.builder())
    }


    @Test
    fun cm101M() = rewriteRun(
        cobol(getNistSource("CM101M.CBL"))
    )

    @Test
    fun cm102M() = rewriteRun(
        cobol(getNistSource("CM102M.CBL"))
    )

    @Test
    fun cm103M() = rewriteRun(
        cobol(getNistSource("CM103M.CBL"))
    )

    @Test
    fun cm104M() = rewriteRun(
        cobol(getNistSource("CM104M.CBL"))
    )

    @Test
    fun cm105M() = rewriteRun(
        cobol(getNistSource("CM105M.CBL"))
    )

    @Test
    fun cm201M() = rewriteRun(
        cobol(getNistSource("CM201M.CBL"))
    )

    @Test
    fun cm202M() = rewriteRun(
        cobol(getNistSource("CM202M.CBL"))
    )

    @Test
    fun cm303M() = rewriteRun(
        cobol(getNistSource("CM303M.CBL"))
    )

    @Test
    fun cm401M() = rewriteRun(
        cobol(getNistSource("CM401M.CBL"))
    )

    @Test
    fun db101A() = rewriteRun(
        cobol(getNistSource("DB101A.CBL"))
    )

    @Test
    fun db102A() = rewriteRun(
        cobol(getNistSource("DB102A.CBL"))
    )

    @Test
    fun db103M() = rewriteRun(
        cobol(getNistSource("DB103M.CBL"))
    )

    @Test
    fun db104A() = rewriteRun(
        cobol(getNistSource("DB104A.CBL"))
    )

    @Test
    fun db105A() = rewriteRun(
        cobol(getNistSource("DB105A.CBL"))
    )

    @Test
    fun db201A() = rewriteRun(
        cobol(getNistSource("DB201A.CBL"))
    )

    @Test
    fun db202A() = rewriteRun(
        cobol(getNistSource("DB202A.CBL"))
    )

    @Test
    fun db203A() = rewriteRun(
        cobol(getNistSource("DB203A.CBL"))
    )

    @Test
    fun db204A() = rewriteRun(
        cobol(getNistSource("DB204A.CBL"))
    )

    @Test
    fun db205A() = rewriteRun(
        cobol(getNistSource("DB205A.CBL"))
    )

    @Test
    fun db301M() = rewriteRun(
        cobol(getNistSource("DB301M.CBL"))
    )

    @Test
    fun db302M() = rewriteRun(
        cobol(getNistSource("DB302M.CBL"))
    )

    @Test
    fun db303M() = rewriteRun(
        cobol(getNistSource("DB303M.CBL"))
    )

    @Test
    fun db304M() = rewriteRun(
        cobol(getNistSource("DB304M.CBL"))
    )

    @Test
    fun db305M() = rewriteRun(
        cobol(getNistSource("DB305M.CBL"))
    )

    @Test
    fun exec85() = rewriteRun(
        cobol(getNistSource("EXEC85.CBL"))
    )

    @Test
    fun ic101A() = rewriteRun(
        cobol(getNistSource("IC101A.CBL"))
    )

    @Test
    fun ic102A() = rewriteRun(
        cobol(getNistSource("IC102A.CBL"))
    )

    @Test
    fun ic103A() = rewriteRun(
        cobol(getNistSource("IC103A.CBL"))
    )

    @Test
    fun ic104A() = rewriteRun(
        cobol(getNistSource("IC104A.CBL"))
    )

    @Test
    fun ic105A() = rewriteRun(
        cobol(getNistSource("IC105A.CBL"))
    )

    @Test
    fun ic106A() = rewriteRun(
        cobol(getNistSource("IC106A.CBL"))
    )

    @Test
    fun ic107A() = rewriteRun(
        cobol(getNistSource("IC107A.CBL"))
    )

    @Test
    fun ic108A() = rewriteRun(
        cobol(getNistSource("IC108A.CBL"))
    )

    @Test
    fun ic109A() = rewriteRun(
        cobol(getNistSource("IC109A.CBL"))
    )

    @Test
    fun ic110A() = rewriteRun(
        cobol(getNistSource("IC110A.CBL"))
    )

    @Test
    fun ic111A() = rewriteRun(
        cobol(getNistSource("IC111A.CBL"))
    )

    @Test
    fun ic112A() = rewriteRun(
        cobol(getNistSource("IC112A.CBL"))
    )

    @Test
    fun ic113A() = rewriteRun(
        cobol(getNistSource("IC113A.CBL"))
    )

    @Test
    fun ic114A() = rewriteRun(
        cobol(getNistSource("IC114A.CBL"))
    )

    @Test
    fun ic115A() = rewriteRun(
        cobol(getNistSource("IC115A.CBL"))
    )

    @Test
    fun ic116M() = rewriteRun(
        cobol(getNistSource("IC116M.CBL"))
    )

    @Test
    fun ic117M() = rewriteRun(
        cobol(getNistSource("IC117M.CBL"))
    )

    @Test
    fun ic118M() = rewriteRun(
        cobol(getNistSource("IC118M.CBL"))
    )

    @Test
    fun ic201A() = rewriteRun(
        cobol(getNistSource("IC201A.CBL"))
    )

    @Test
    fun ic202A() = rewriteRun(
        cobol(getNistSource("IC202A.CBL"))
    )

    @Test
    fun ic203A() = rewriteRun(
        cobol(getNistSource("IC203A.CBL"))
    )

    @Test
    fun ic204A() = rewriteRun(
        cobol(getNistSource("IC204A.CBL"))
    )

    @Test
    fun ic205A() = rewriteRun(
        cobol(getNistSource("IC205A.CBL"))
    )

    @Test
    fun ic206A() = rewriteRun(
        cobol(getNistSource("IC206A.CBL"))
    )

    @Test
    fun ic207A() = rewriteRun(
        cobol(getNistSource("IC207A.CBL"))
    )

    @Test
    fun ic208A() = rewriteRun(
        cobol(getNistSource("IC208A.CBL"))
    )

    @Test
    fun ic209A() = rewriteRun(
        cobol(getNistSource("IC209A.CBL"))
    )

    @Test
    fun ic210A() = rewriteRun(
        cobol(getNistSource("IC210A.CBL"))
    )

    @Test
    fun ic211A() = rewriteRun(
        cobol(getNistSource("IC211A.CBL"))
    )

    @Test
    fun ic212A() = rewriteRun(
        cobol(getNistSource("IC212A.CBL"))
    )

    @Test
    fun ic213A() = rewriteRun(
        cobol(getNistSource("IC213A.CBL"))
    )

    @Test
    fun ic214A() = rewriteRun(
        cobol(getNistSource("IC214A.CBL"))
    )

    @Test
    fun ic215A() = rewriteRun(
        cobol(getNistSource("IC215A.CBL"))
    )

    @Test
    fun ic216A() = rewriteRun(
        cobol(getNistSource("IC216A.CBL"))
    )

    @Test
    fun ic217A() = rewriteRun(
        cobol(getNistSource("IC217A.CBL"))
    )

    @Test
    fun ic222A() = rewriteRun(
        cobol(getNistSource("IC222A.CBL"))
    )

    @Test
    fun ic223A() = rewriteRun(
        cobol(getNistSource("IC223A.CBL"))
    )

    @Test
    fun ic224A() = rewriteRun(
        cobol(getNistSource("IC224A.CBL"))
    )

    @Test
    fun ic225A() = rewriteRun(
        cobol(getNistSource("IC225A.CBL"))
    )

    @Test
    fun ic226A() = rewriteRun(
        cobol(getNistSource("IC226A.CBL"))
    )

    @Test
    fun ic227A() = rewriteRun(
        cobol(getNistSource("IC227A.CBL"))
    )

    @Test
    fun ic228A() = rewriteRun(
        cobol(getNistSource("IC228A.CBL"))
    )

    @Test
    fun ic233A() = rewriteRun(
        cobol(getNistSource("IC233A.CBL"))
    )

    @Test
    fun ic234A() = rewriteRun(
        cobol(getNistSource("IC234A.CBL"))
    )

    @Test
    fun ic235A() = rewriteRun(
        cobol(getNistSource("IC235A.CBL"))
    )

    @Test
    fun ic237A() = rewriteRun(
        cobol(getNistSource("IC237A.CBL"))
    )

    @Test
    fun ic401M() = rewriteRun(
        cobol(getNistSource("IC401M.CBL"))
    )

    @Test
    fun if101A() = rewriteRun(
        cobol(getNistSource("IF101A.CBL"))
    )

    @Test
    fun if102A() = rewriteRun(
        cobol(getNistSource("IF102A.CBL"))
    )

    @Test
    fun if103A() = rewriteRun(
        cobol(getNistSource("IF103A.CBL"))
    )

    @Test
    fun if104A() = rewriteRun(
        cobol(getNistSource("IF104A.CBL"))
    )

    @Test
    fun if105A() = rewriteRun(
        cobol(getNistSource("IF105A.CBL"))
    )

    @Test
    fun if106A() = rewriteRun(
        cobol(getNistSource("IF106A.CBL"))
    )

    @Test
    fun if107A() = rewriteRun(
        cobol(getNistSource("IF107A.CBL"))
    )

    @Test
    fun if108A() = rewriteRun(
        cobol(getNistSource("IF108A.CBL"))
    )

    @Test
    fun if109A() = rewriteRun(
        cobol(getNistSource("IF109A.CBL"))
    )

    @Test
    fun if110A() = rewriteRun(
        cobol(getNistSource("IF110A.CBL"))
    )

    @Test
    fun if111A() = rewriteRun(
        cobol(getNistSource("IF111A.CBL"))
    )

    @Test
    fun if112A() = rewriteRun(
        cobol(getNistSource("IF112A.CBL"))
    )

    @Test
    fun if113A() = rewriteRun(
        cobol(getNistSource("IF113A.CBL"))
    )

    @Test
    fun if114A() = rewriteRun(
        cobol(getNistSource("IF114A.CBL"))
    )

    @Test
    fun if115A() = rewriteRun(
        cobol(getNistSource("IF115A.CBL"))
    )

    @Test
    fun if116A() = rewriteRun(
        cobol(getNistSource("IF116A.CBL"))
    )

    @Test
    fun if117A() = rewriteRun(
        cobol(getNistSource("IF117A.CBL"))
    )

    @Test
    fun if118A() = rewriteRun(
        cobol(getNistSource("IF118A.CBL"))
    )

    @Test
    fun if119A() = rewriteRun(
        cobol(getNistSource("IF119A.CBL"))
    )

    @Test
    fun if120A() = rewriteRun(
        cobol(getNistSource("IF120A.CBL"))
    )

    @Test
    fun if121A() = rewriteRun(
        cobol(getNistSource("IF121A.CBL"))
    )

    @Test
    fun if122A() = rewriteRun(
        cobol(getNistSource("IF122A.CBL"))
    )

    @Test
    fun if123A() = rewriteRun(
        cobol(getNistSource("IF123A.CBL"))
    )

    @Test
    fun if124A() = rewriteRun(
        cobol(getNistSource("IF124A.CBL"))
    )

    @Test
    fun if125A() = rewriteRun(
        cobol(getNistSource("IF125A.CBL"))
    )

    @Test
    fun if126A() = rewriteRun(
        cobol(getNistSource("IF126A.CBL"))
    )

    @Test
    fun if127A() = rewriteRun(
        cobol(getNistSource("IF127A.CBL"))
    )

    @Test
    fun if128A() = rewriteRun(
        cobol(getNistSource("IF128A.CBL"))
    )

    @Test
    fun if129A() = rewriteRun(
        cobol(getNistSource("IF129A.CBL"))
    )

    @Test
    fun if130A() = rewriteRun(
        cobol(getNistSource("IF130A.CBL"))
    )

    @Test
    fun if131A() = rewriteRun(
        cobol(getNistSource("IF131A.CBL"))
    )

    @Test
    fun if132A() = rewriteRun(
        cobol(getNistSource("IF132A.CBL"))
    )

    @Test
    fun if133A() = rewriteRun(
        cobol(getNistSource("IF133A.CBL"))
    )

    @Test
    fun if134A() = rewriteRun(
        cobol(getNistSource("IF134A.CBL"))
    )

    @Test
    fun if135A() = rewriteRun(
        cobol(getNistSource("IF135A.CBL"))
    )

    @Test
    fun if136A() = rewriteRun(
        cobol(getNistSource("IF136A.CBL"))
    )

    @Test
    fun if137A() = rewriteRun(
        cobol(getNistSource("IF137A.CBL"))
    )

    @Test
    fun if138A() = rewriteRun(
        cobol(getNistSource("IF138A.CBL"))
    )

    @Test
    fun if139A() = rewriteRun(
        cobol(getNistSource("IF139A.CBL"))
    )

    @Test
    fun if140A() = rewriteRun(
        cobol(getNistSource("IF140A.CBL"))
    )

    @Test
    fun if141A() = rewriteRun(
        cobol(getNistSource("IF141A.CBL"))
    )

    @Test
    fun if142A() = rewriteRun(
        cobol(getNistSource("IF142A.CBL"))
    )

    @Test
    fun if401M() = rewriteRun(
        cobol(getNistSource("IF401M.CBL"))
    )

    @Test
    fun if402M() = rewriteRun(
        cobol(getNistSource("IF402M.CBL"))
    )

    @Test
    fun if403M() = rewriteRun(
        cobol(getNistSource("IF403M.CBL"))
    )

    @Test
    fun ix101A() = rewriteRun(
        cobol(getNistSource("IX101A.CBL"))
    )

    @Test
    fun ix102A() = rewriteRun(
        cobol(getNistSource("IX102A.CBL"))
    )

    @Test
    fun ix103A() = rewriteRun(
        cobol(getNistSource("IX103A.CBL"))
    )

    @Test
    fun ix104A() = rewriteRun(
        cobol(getNistSource("IX104A.CBL"))
    )

    @Test
    fun ix105A() = rewriteRun(
        cobol(getNistSource("IX105A.CBL"))
    )

    @Test
    fun ix106A() = rewriteRun(
        cobol(getNistSource("IX106A.CBL"))
    )

    @Test
    fun ix107A() = rewriteRun(
        cobol(getNistSource("IX107A.CBL"))
    )

    @Test
    fun ix108A() = rewriteRun(
        cobol(getNistSource("IX108A.CBL"))
    )

    @Test
    fun ix109A() = rewriteRun(
        cobol(getNistSource("IX109A.CBL"))
    )

    @Test
    fun ix110A() = rewriteRun(
        cobol(getNistSource("IX110A.CBL"))
    )

    @Test
    fun ix111A() = rewriteRun(
        cobol(getNistSource("IX111A.CBL"))
    )

    @Test
    fun ix112A() = rewriteRun(
        cobol(getNistSource("IX112A.CBL"))
    )

    @Test
    fun ix113A() = rewriteRun(
        cobol(getNistSource("IX113A.CBL"))
    )

    @Test
    fun ix114A() = rewriteRun(
        cobol(getNistSource("IX114A.CBL"))
    )

    @Test
    fun ix115A() = rewriteRun(
        cobol(getNistSource("IX115A.CBL"))
    )

    @Test
    fun ix116A() = rewriteRun(
        cobol(getNistSource("IX116A.CBL"))
    )

    @Test
    fun ix117A() = rewriteRun(
        cobol(getNistSource("IX117A.CBL"))
    )

    @Test
    fun ix118A() = rewriteRun(
        cobol(getNistSource("IX118A.CBL"))
    )

    @Test
    fun ix119A() = rewriteRun(
        cobol(getNistSource("IX119A.CBL"))
    )

    @Test
    fun ix120A() = rewriteRun(
        cobol(getNistSource("IX120A.CBL"))
    )

    @Test
    fun ix121A() = rewriteRun(
        cobol(getNistSource("IX121A.CBL"))
    )

    @Test
    fun ix201A() = rewriteRun(
        cobol(getNistSource("IX201A.CBL"))
    )

    @Test
    fun ix202A() = rewriteRun(
        cobol(getNistSource("IX202A.CBL"))
    )

    @Test
    fun ix203A() = rewriteRun(
        cobol(getNistSource("IX203A.CBL"))
    )

    @Test
    fun ix204A() = rewriteRun(
        cobol(getNistSource("IX204A.CBL"))
    )

    @Test
    fun ix205A() = rewriteRun(
        cobol(getNistSource("IX205A.CBL"))
    )

    @Test
    fun ix206A() = rewriteRun(
        cobol(getNistSource("IX206A.CBL"))
    )

    @Test
    fun ix207A() = rewriteRun(
        cobol(getNistSource("IX207A.CBL"))
    )

    @Test
    fun ix208A() = rewriteRun(
        cobol(getNistSource("IX208A.CBL"))
    )

    @Test
    fun ix209A() = rewriteRun(
        cobol(getNistSource("IX209A.CBL"))
    )

    @Test
    fun ix210A() = rewriteRun(
        cobol(getNistSource("IX210A.CBL"))
    )

    @Test
    fun ix211A() = rewriteRun(
        cobol(getNistSource("IX211A.CBL"))
    )

    @Test
    fun ix212A() = rewriteRun(
        cobol(getNistSource("IX212A.CBL"))
    )

    @Test
    fun ix213A() = rewriteRun(
        cobol(getNistSource("IX213A.CBL"))
    )

    @Test
    fun ix214A() = rewriteRun(
        cobol(getNistSource("IX214A.CBL"))
    )

    @Test
    fun ix215A() = rewriteRun(
        cobol(getNistSource("IX215A.CBL"))
    )

    @Test
    fun ix216A() = rewriteRun(
        cobol(getNistSource("IX216A.CBL"))
    )

    @Test
    fun ix217A() = rewriteRun(
        cobol(getNistSource("IX217A.CBL"))
    )

    @Test
    fun ix218A() = rewriteRun(
        cobol(getNistSource("IX218A.CBL"))
    )

    @Test
    fun ix301M() = rewriteRun(
        cobol(getNistSource("IX301M.CBL"))
    )

    @Test
    fun ix302M() = rewriteRun(
        cobol(getNistSource("IX302M.CBL"))
    )

    @Test
    fun ix401M() = rewriteRun(
        cobol(getNistSource("IX401M.CBL"))
    )

    @Test
    fun nc101A() = rewriteRun(
        cobol(getNistSource("NC101A.CBL"))
    )

    @Test
    fun nc102A() = rewriteRun(
        cobol(getNistSource("NC102A.CBL"))
    )

    @Test
    fun nc103A() = rewriteRun(
        cobol(getNistSource("NC103A.CBL"))
    )

    @Test
    fun nc104A() = rewriteRun(
        cobol(getNistSource("NC104A.CBL"))
    )

    @Test
    fun nc105A() = rewriteRun(
        cobol(getNistSource("NC105A.CBL"))
    )

    @Test
    fun nc106A() = rewriteRun(
        cobol(getNistSource("NC106A.CBL"))
    )

    @Test
    fun nc107A() = rewriteRun(
        cobol(getNistSource("NC107A.CBL"))
    )

    @Test
    fun nc108M() = rewriteRun(
        cobol(getNistSource("NC108M.CBL"))
    )

    @Test
    fun nc109M() = rewriteRun(
        cobol(getNistSource("NC109M.CBL"))
    )

    @Test
    fun nc110M() = rewriteRun(
        cobol(getNistSource("NC110M.CBL"))
    )

    @Test
    fun nc111A() = rewriteRun(
        cobol(getNistSource("NC111A.CBL"))
    )

    @Test
    fun nc112A() = rewriteRun(
        cobol(getNistSource("NC112A.CBL"))
    )

    @Test
    fun nc113M() = rewriteRun(
        cobol(getNistSource("NC113M.CBL"))
    )

    @Disabled("Requires / indicator for comment entries that are moved to the top of the page.")
    @Test
    fun nc114M() = rewriteRun(
        cobol(getNistSource("NC114M.CBL"))
    )

    @Test
    fun nc115A() = rewriteRun(
        cobol(getNistSource("NC115A.CBL"))
    )

    @Test
    fun nc116A() = rewriteRun(
        cobol(getNistSource("NC116A.CBL"))
    )

    @Test
    fun nc117A() = rewriteRun(
        cobol(getNistSource("NC117A.CBL"))
    )

    @Test
    fun nc118A() = rewriteRun(
        cobol(getNistSource("NC118A.CBL"))
    )

    @Test
    fun nc119A() = rewriteRun(
        cobol(getNistSource("NC119A.CBL"))
    )

    @Test
    fun nc120A() = rewriteRun(
        cobol(getNistSource("NC120A.CBL"))
    )

    @Test
    fun nc121M() = rewriteRun(
        cobol(getNistSource("NC121M.CBL"))
    )

    @Test
    fun nc122A() = rewriteRun(
        cobol(getNistSource("NC122A.CBL"))
    )

    @Test
    fun nc123A() = rewriteRun(
        cobol(getNistSource("NC123A.CBL"))
    )

    @Test
    fun nc124A() = rewriteRun(
        cobol(getNistSource("NC124A.CBL"))
    )

    @Test
    fun nc125A() = rewriteRun(
        cobol(getNistSource("NC125A.CBL"))
    )

    @Test
    fun nc126A() = rewriteRun(
        cobol(getNistSource("NC126A.CBL"))
    )

    @Test
    fun nc127A() = rewriteRun(
        cobol(getNistSource("NC127A.CBL"))
    )

    @Test
    fun nc131A() = rewriteRun(
        cobol(getNistSource("NC131A.CBL"))
    )

    @Test
    fun nc132A() = rewriteRun(
        cobol(getNistSource("NC132A.CBL"))
    )

    @Test
    fun nc133A() = rewriteRun(
        cobol(getNistSource("NC133A.CBL"))
    )

    @Test
    fun nc134A() = rewriteRun(
        cobol(getNistSource("NC134A.CBL"))
    )

    @Test
    fun nc135A() = rewriteRun(
        cobol(getNistSource("NC135A.CBL"))
    )

    @Test
    fun nc136A() = rewriteRun(
        cobol(getNistSource("NC136A.CBL"))
    )

    @Test
    fun nc137A() = rewriteRun(
        cobol(getNistSource("NC137A.CBL"))
    )

    @Test
    fun nc138A() = rewriteRun(
        cobol(getNistSource("NC138A.CBL"))
    )

    @Test
    fun nc139A() = rewriteRun(
        cobol(getNistSource("NC139A.CBL"))
    )

    @Test
    fun nc140A() = rewriteRun(
        cobol(getNistSource("NC140A.CBL"))
    )

    @Test
    fun nc141A() = rewriteRun(
        cobol(getNistSource("NC141A.CBL"))
    )

    @Test
    fun nc171A() = rewriteRun(
        cobol(getNistSource("NC171A.CBL"))
    )

    @Test
    fun nc172A() = rewriteRun(
        cobol(getNistSource("NC172A.CBL"))
    )

    @Test
    fun nc173A() = rewriteRun(
        cobol(getNistSource("NC173A.CBL"))
    )

    @Test
    fun nc174A() = rewriteRun(
        cobol(getNistSource("NC174A.CBL"))
    )

    @Test
    fun nc175A() = rewriteRun(
        cobol(getNistSource("NC175A.CBL"))
    )

    @Test
    fun nc176A() = rewriteRun(
        cobol(getNistSource("NC176A.CBL"))
    )

    @Test
    fun nc177A() = rewriteRun(
        cobol(getNistSource("NC177A.CBL"))
    )

    @Test
    fun nc201A() = rewriteRun(
        cobol(getNistSource("NC201A.CBL"))
    )

    @Test
    fun nc202A() = rewriteRun(
        cobol(getNistSource("NC202A.CBL"))
    )

    @Test
    fun nc203A() = rewriteRun(
        cobol(getNistSource("NC203A.CBL"))
    )

    @Test
    fun nc204M() = rewriteRun(
        cobol(getNistSource("NC204M.CBL"))
    )

    @Disabled("Requires continuation between tokens.")
    @Test
    fun nc205A() = rewriteRun(
        cobol(getNistSource("NC205A.CBL"))
    )

    @Test
    fun nc206A() = rewriteRun(
        cobol(getNistSource("NC206A.CBL"))
    )

    @Test
    fun nc207A() = rewriteRun(
        cobol(getNistSource("NC207A.CBL"))
    )

    @Test
    fun nc208A() = rewriteRun(
        cobol(getNistSource("NC208A.CBL"))
    )

    @Test
    fun nc209A() = rewriteRun(
        cobol(getNistSource("NC209A.CBL"))
    )

    @Test
    fun nc210A() = rewriteRun(
        cobol(getNistSource("NC210A.CBL"))
    )

    @Test
    fun nc211A() = rewriteRun(
        cobol(getNistSource("NC211A.CBL"))
    )

    @Test
    fun nc214M() = rewriteRun(
        cobol(getNistSource("NC214M.CBL"))
    )

    @Test
    fun nc215A() = rewriteRun(
        cobol(getNistSource("NC215A.CBL"))
    )

    @Test
    fun nc216A() = rewriteRun(
        cobol(getNistSource("NC216A.CBL"))
    )

    @Test
    fun nc217A() = rewriteRun(
        cobol(getNistSource("NC217A.CBL"))
    )

    @Test
    fun nc218A() = rewriteRun(
        cobol(getNistSource("NC218A.CBL"))
    )

    @Test
    fun nc219A() = rewriteRun(
        cobol(getNistSource("NC219A.CBL"))
    )

    @Test
    fun nc220M() = rewriteRun(
        cobol(getNistSource("NC220M.CBL"))
    )

    @Test
    fun nc221A() = rewriteRun(
        cobol(getNistSource("NC221A.CBL"))
    )

    @Test
    fun nc222A() = rewriteRun(
        cobol(getNistSource("NC222A.CBL"))
    )

    @Test
    fun nc223A() = rewriteRun(
        cobol(getNistSource("NC223A.CBL"))
    )

    @Test
    fun nc224A() = rewriteRun(
        cobol(getNistSource("NC224A.CBL"))
    )

    @Test
    fun nc225A() = rewriteRun(
        cobol(getNistSource("NC225A.CBL"))
    )

    @Test
    fun nc231A() = rewriteRun(
        cobol(getNistSource("NC231A.CBL"))
    )

    @Test
    fun nc232A() = rewriteRun(
        cobol(getNistSource("NC232A.CBL"))
    )

    @Test
    fun nc233A() = rewriteRun(
        cobol(getNistSource("NC233A.CBL"))
    )

    @Test
    fun nc234A() = rewriteRun(
        cobol(getNistSource("NC234A.CBL"))
    )

    @Test
    fun nc235A() = rewriteRun(
        cobol(getNistSource("NC235A.CBL"))
    )

    @Test
    fun nc236A() = rewriteRun(
        cobol(getNistSource("NC236A.CBL"))
    )

    @Test
    fun nc237A() = rewriteRun(
        cobol(getNistSource("NC237A.CBL"))
    )

    @Test
    fun nc238A() = rewriteRun(
        cobol(getNistSource("NC238A.CBL"))
    )

    @Test
    fun nc239A() = rewriteRun(
        cobol(getNistSource("NC239A.CBL"))
    )

    @Test
    fun nc240A() = rewriteRun(
        cobol(getNistSource("NC240A.CBL"))
    )

    @Test
    fun nc241A() = rewriteRun(
        cobol(getNistSource("NC241A.CBL"))
    )

    @Test
    fun nc242A() = rewriteRun(
        cobol(getNistSource("NC242A.CBL"))
    )

    @Test
    fun nc243A() = rewriteRun(
        cobol(getNistSource("NC243A.CBL"))
    )

    @Test
    fun nc244A() = rewriteRun(
        cobol(getNistSource("NC244A.CBL"))
    )

    @Test
    fun nc245A() = rewriteRun(
        cobol(getNistSource("NC245A.CBL"))
    )

    @Test
    fun nc246A() = rewriteRun(
        cobol(getNistSource("NC246A.CBL"))
    )

    @Test
    fun nc247A() = rewriteRun(
        cobol(getNistSource("NC247A.CBL"))
    )

    @Test
    fun nc248A() = rewriteRun(
        cobol(getNistSource("NC248A.CBL"))
    )

    @Test
    fun nc250A() = rewriteRun(
        cobol(getNistSource("NC250A.CBL"))
    )

    @Test
    fun nc251A() = rewriteRun(
        cobol(getNistSource("NC251A.CBL"))
    )

    @Test
    fun nc252A() = rewriteRun(
        cobol(getNistSource("NC252A.CBL"))
    )

    @Test
    fun nc253A() = rewriteRun(
        cobol(getNistSource("NC253A.CBL"))
    )

    @Test
    fun nc254A() = rewriteRun(
        cobol(getNistSource("NC254A.CBL"))
    )

    @Test
    fun nc302M() = rewriteRun(
        cobol(getNistSource("NC302M.CBL"))
    )

    @Test
    fun nc303M() = rewriteRun(
        cobol(getNistSource("NC303M.CBL"))
    )

    @Disabled("Requires continuation between tokens.")
    @Test
    fun nc401M() = rewriteRun(
        cobol(getNistSource("NC401M.CBL"))
    )

    @Test
    fun obic1A() = rewriteRun(
        cobol(getNistSource("OBIC1A.CBL"))
    )

    @Test
    fun obic2A() = rewriteRun(
        cobol(getNistSource("OBIC2A.CBL"))
    )

    @Test
    fun obic3A() = rewriteRun(
        cobol(getNistSource("OBIC3A.CBL"))
    )

    @Test
    fun obnc1M() = rewriteRun(
        cobol(getNistSource("OBNC1M.CBL"))
    )

    @Test
    fun obnc2M() = rewriteRun(
        cobol(getNistSource("OBNC2M.CBL"))
    )

    @Test
    fun obsq1A() = rewriteRun(
        cobol(getNistSource("OBSQ1A.CBL"))
    )

    @Test
    fun obsq3A() = rewriteRun(
        cobol(getNistSource("OBSQ3A.CBL"))
    )

    @Test
    fun obsq4A() = rewriteRun(
        cobol(getNistSource("OBSQ4A.CBL"))
    )

    @Test
    fun obsq5A() = rewriteRun(
        cobol(getNistSource("OBSQ5A.CBL"))
    )

    @Test
    fun rl101A() = rewriteRun(
        cobol(getNistSource("RL101A.CBL"))
    )

    @Test
    fun rl102A() = rewriteRun(
        cobol(getNistSource("RL102A.CBL"))
    )

    @Test
    fun rl103A() = rewriteRun(
        cobol(getNistSource("RL103A.CBL"))
    )

    @Test
    fun rl104A() = rewriteRun(
        cobol(getNistSource("RL104A.CBL"))
    )

    @Test
    fun rl105A() = rewriteRun(
        cobol(getNistSource("RL105A.CBL"))
    )

    @Test
    fun rl106A() = rewriteRun(
        cobol(getNistSource("RL106A.CBL"))
    )

    @Test
    fun rl107A() = rewriteRun(
        cobol(getNistSource("RL107A.CBL"))
    )

    @Test
    fun rl108A() = rewriteRun(
        cobol(getNistSource("RL108A.CBL"))
    )

    @Test
    fun rl109A() = rewriteRun(
        cobol(getNistSource("RL109A.CBL"))
    )

    @Test
    fun rl110A() = rewriteRun(
        cobol(getNistSource("RL110A.CBL"))
    )

    @Test
    fun rl111A() = rewriteRun(
        cobol(getNistSource("RL111A.CBL"))
    )

    @Test
    fun rl112A() = rewriteRun(
        cobol(getNistSource("RL112A.CBL"))
    )

    @Test
    fun rl113A() = rewriteRun(
        cobol(getNistSource("RL113A.CBL"))
    )

    @Test
    fun rl114A() = rewriteRun(
        cobol(getNistSource("RL114A.CBL"))
    )

    @Test
    fun rl115A() = rewriteRun(
        cobol(getNistSource("RL115A.CBL"))
    )

    @Test
    fun rl116A() = rewriteRun(
        cobol(getNistSource("RL116A.CBL"))
    )

    @Test
    fun rl117A() = rewriteRun(
        cobol(getNistSource("RL117A.CBL"))
    )

    @Test
    fun rl118A() = rewriteRun(
        cobol(getNistSource("RL118A.CBL"))
    )

    @Test
    fun rl119A() = rewriteRun(
        cobol(getNistSource("RL119A.CBL"))
    )

    @Test
    fun rl201A() = rewriteRun(
        cobol(getNistSource("RL201A.CBL"))
    )

    @Test
    fun rl202A() = rewriteRun(
        cobol(getNistSource("RL202A.CBL"))
    )

    @Test
    fun rl203A() = rewriteRun(
        cobol(getNistSource("RL203A.CBL"))
    )

    @Test
    fun rl204A() = rewriteRun(
        cobol(getNistSource("RL204A.CBL"))
    )

    @Test
    fun rl205A() = rewriteRun(
        cobol(getNistSource("RL205A.CBL"))
    )

    @Test
    fun rl206A() = rewriteRun(
        cobol(getNistSource("RL206A.CBL"))
    )

    @Test
    fun rl207A() = rewriteRun(
        cobol(getNistSource("RL207A.CBL"))
    )

    @Test
    fun rl208A() = rewriteRun(
        cobol(getNistSource("RL208A.CBL"))
    )

    @Test
    fun rl209A() = rewriteRun(
        cobol(getNistSource("RL209A.CBL"))
    )

    @Test
    fun rl210A() = rewriteRun(
        cobol(getNistSource("RL210A.CBL"))
    )

    @Test
    fun rl211A() = rewriteRun(
        cobol(getNistSource("RL211A.CBL"))
    )

    @Test
    fun rl212A() = rewriteRun(
        cobol(getNistSource("RL212A.CBL"))
    )

    @Test
    fun rl213A() = rewriteRun(
        cobol(getNistSource("RL213A.CBL"))
    )

    @Test
    fun rl301M() = rewriteRun(
        cobol(getNistSource("RL301M.CBL"))
    )

    @Test
    fun rl302M() = rewriteRun(
        cobol(getNistSource("RL302M.CBL"))
    )

    @Test
    fun rl401M() = rewriteRun(
        cobol(getNistSource("RL401M.CBL"))
    )

    @Test
    fun rw101A() = rewriteRun(
        cobol(getNistSource("RW101A.CBL"))
    )

    @Test
    fun rw102A() = rewriteRun(
        cobol(getNistSource("RW102A.CBL"))
    )

    @Test
    fun rw103A() = rewriteRun(
        cobol(getNistSource("RW103A.CBL"))
    )

    @Test
    fun rw104A() = rewriteRun(
        cobol(getNistSource("RW104A.CBL"))
    )

    @Test
    fun rw301M() = rewriteRun(
        cobol(getNistSource("RW301M.CBL"))
    )

    @Test
    fun rw302M() = rewriteRun(
        cobol(getNistSource("RW302M.CBL"))
    )

    @Test
    fun sg101A() = rewriteRun(
        cobol(getNistSource("SG101A.CBL"))
    )

    @Test
    fun sg102A() = rewriteRun(
        cobol(getNistSource("SG102A.CBL"))
    )

    @Test
    fun sg103A() = rewriteRun(
        cobol(getNistSource("SG103A.CBL"))
    )

    @Test
    fun sg104A() = rewriteRun(
        cobol(getNistSource("SG104A.CBL"))
    )

    @Test
    fun sg105A() = rewriteRun(
        cobol(getNistSource("SG105A.CBL"))
    )

    @Test
    fun sg106A() = rewriteRun(
        cobol(getNistSource("SG106A.CBL"))
    )

    @Test
    fun sg201A() = rewriteRun(
        cobol(getNistSource("SG201A.CBL"))
    )

    @Test
    fun sg202A() = rewriteRun(
        cobol(getNistSource("SG202A.CBL"))
    )

    @Test
    fun sg203A() = rewriteRun(
        cobol(getNistSource("SG203A.CBL"))
    )

    @Test
    fun sg204A() = rewriteRun(
        cobol(getNistSource("SG204A.CBL"))
    )

    @Test
    fun sm102A() = rewriteRun(
        cobol(getNistSource("SM102A.CBL"))
    )

    @Test
    fun sm104A() = rewriteRun(
        cobol(getNistSource("SM104A.CBL"))
    )

    @Test
    fun sm204A() = rewriteRun(
        cobol(getNistSource("SM204A.CBL"))
    )

    @Test
    fun sq101M() = rewriteRun(
        cobol(getNistSource("SQ101M.CBL"))
    )

    @Test
    fun sq102A() = rewriteRun(
        cobol(getNistSource("SQ102A.CBL"))
    )

    @Test
    fun sq103A() = rewriteRun(
        cobol(getNistSource("SQ103A.CBL"))
    )

    @Test
    fun sq104A() = rewriteRun(
        cobol(getNistSource("SQ104A.CBL"))
    )

    @Test
    fun sq105A() = rewriteRun(
        cobol(getNistSource("SQ105A.CBL"))
    )

    @Test
    fun sq106A() = rewriteRun(
        cobol(getNistSource("SQ106A.CBL"))
    )

    @Test
    fun sq107A() = rewriteRun(
        cobol(getNistSource("SQ107A.CBL"))
    )

    @Test
    fun sq108A() = rewriteRun(
        cobol(getNistSource("SQ108A.CBL"))
    )

    @Test
    fun sq109M() = rewriteRun(
        cobol(getNistSource("SQ109M.CBL"))
    )

    @Test
    fun sq110M() = rewriteRun(
        cobol(getNistSource("SQ110M.CBL"))
    )

    @Test
    fun sq111A() = rewriteRun(
        cobol(getNistSource("SQ111A.CBL"))
    )

    @Test
    fun sq112A() = rewriteRun(
        cobol(getNistSource("SQ112A.CBL"))
    )

    @Test
    fun sq113A() = rewriteRun(
        cobol(getNistSource("SQ113A.CBL"))
    )

    @Test
    fun sq114A() = rewriteRun(
        cobol(getNistSource("SQ114A.CBL"))
    )

    @Test
    fun sq115A() = rewriteRun(
        cobol(getNistSource("SQ115A.CBL"))
    )

    @Test
    fun sq116A() = rewriteRun(
        cobol(getNistSource("SQ116A.CBL"))
    )

    @Test
    fun sq117A() = rewriteRun(
        cobol(getNistSource("SQ117A.CBL"))
    )

    @Test
    fun sq121A() = rewriteRun(
        cobol(getNistSource("SQ121A.CBL"))
    )

    @Test
    fun sq122A() = rewriteRun(
        cobol(getNistSource("SQ122A.CBL"))
    )

    @Test
    fun sq123A() = rewriteRun(
        cobol(getNistSource("SQ123A.CBL"))
    )

    @Test
    fun sq124A() = rewriteRun(
        cobol(getNistSource("SQ124A.CBL"))
    )

    @Test
    fun sq125A() = rewriteRun(
        cobol(getNistSource("SQ125A.CBL"))
    )

    @Test
    fun sq126A() = rewriteRun(
        cobol(getNistSource("SQ126A.CBL"))
    )

    @Test
    fun sq127A() = rewriteRun(
        cobol(getNistSource("SQ127A.CBL"))
    )

    @Test
    fun sq128A() = rewriteRun(
        cobol(getNistSource("SQ128A.CBL"))
    )

    @Test
    fun sq129A() = rewriteRun(
        cobol(getNistSource("SQ129A.CBL"))
    )

    @Test
    fun sq130A() = rewriteRun(
        cobol(getNistSource("SQ130A.CBL"))
    )

    @Test
    fun sq131A() = rewriteRun(
        cobol(getNistSource("SQ131A.CBL"))
    )

    @Test
    fun sq132A() = rewriteRun(
        cobol(getNistSource("SQ132A.CBL"))
    )

    @Test
    fun sq133A() = rewriteRun(
        cobol(getNistSource("SQ133A.CBL"))
    )

    @Test
    fun sq134A() = rewriteRun(
        cobol(getNistSource("SQ134A.CBL"))
    )

    @Test
    fun sq135A() = rewriteRun(
        cobol(getNistSource("SQ135A.CBL"))
    )

    @Test
    fun sq136A() = rewriteRun(
        cobol(getNistSource("SQ136A.CBL"))
    )

    @Test
    fun sq137A() = rewriteRun(
        cobol(getNistSource("SQ137A.CBL"))
    )

    @Test
    fun sq138A() = rewriteRun(
        cobol(getNistSource("SQ138A.CBL"))
    )

    @Test
    fun sq139A() = rewriteRun(
        cobol(getNistSource("SQ139A.CBL"))
    )

    @Test
    fun sq140A() = rewriteRun(
        cobol(getNistSource("SQ140A.CBL"))
    )

    @Test
    fun sq141A() = rewriteRun(
        cobol(getNistSource("SQ141A.CBL"))
    )

    @Test
    fun sq142A() = rewriteRun(
        cobol(getNistSource("SQ142A.CBL"))
    )

    @Test
    fun sq143A() = rewriteRun(
        cobol(getNistSource("SQ143A.CBL"))
    )

    @Test
    fun sq144A() = rewriteRun(
        cobol(getNistSource("SQ144A.CBL"))
    )

    @Test
    fun sq146A() = rewriteRun(
        cobol(getNistSource("SQ146A.CBL"))
    )

    @Test
    fun sq147A() = rewriteRun(
        cobol(getNistSource("SQ147A.CBL"))
    )

    @Test
    fun sq148A() = rewriteRun(
        cobol(getNistSource("SQ148A.CBL"))
    )

    @Test
    fun sq149A() = rewriteRun(
        cobol(getNistSource("SQ149A.CBL"))
    )

    @Test
    fun sq150A() = rewriteRun(
        cobol(getNistSource("SQ150A.CBL"))
    )

    @Test
    fun sq151A() = rewriteRun(
        cobol(getNistSource("SQ151A.CBL"))
    )

    @Test
    fun sq152A() = rewriteRun(
        cobol(getNistSource("SQ152A.CBL"))
    )

    @Test
    fun sq153A() = rewriteRun(
        cobol(getNistSource("SQ153A.CBL"))
    )

    @Test
    fun sq154A() = rewriteRun(
        cobol(getNistSource("SQ154A.CBL"))
    )

    @Test
    fun sq155A() = rewriteRun(
        cobol(getNistSource("SQ155A.CBL"))
    )

    @Test
    fun sq156A() = rewriteRun(
        cobol(getNistSource("SQ156A.CBL"))
    )

    @Test
    fun sq201M() = rewriteRun(
        cobol(getNistSource("SQ201M.CBL"))
    )

    @Test
    fun sq202A() = rewriteRun(
        cobol(getNistSource("SQ202A.CBL"))
    )

    @Test
    fun sq203A() = rewriteRun(
        cobol(getNistSource("SQ203A.CBL"))
    )

    @Test
    fun sq204A() = rewriteRun(
        cobol(getNistSource("SQ204A.CBL"))
    )

    @Test
    fun sq205A() = rewriteRun(
        cobol(getNistSource("SQ205A.CBL"))
    )

    @Test
    fun sq206A() = rewriteRun(
        cobol(getNistSource("SQ206A.CBL"))
    )

    @Test
    fun sq207M() = rewriteRun(
        cobol(getNistSource("SQ207M.CBL"))
    )

    @Test
    fun sq208M() = rewriteRun(
        cobol(getNistSource("SQ208M.CBL"))
    )

    @Test
    fun sq209M() = rewriteRun(
        cobol(getNistSource("SQ209M.CBL"))
    )

    @Test
    fun sq210M() = rewriteRun(
        cobol(getNistSource("SQ210M.CBL"))
    )

    @Test
    fun sq211A() = rewriteRun(
        cobol(getNistSource("SQ211A.CBL"))
    )

    @Test
    fun sq212A() = rewriteRun(
        cobol(getNistSource("SQ212A.CBL"))
    )

    @Test
    fun sq213A() = rewriteRun(
        cobol(getNistSource("SQ213A.CBL"))
    )

    @Test
    fun sq214A() = rewriteRun(
        cobol(getNistSource("SQ214A.CBL"))
    )

    @Test
    fun sq215A() = rewriteRun(
        cobol(getNistSource("SQ215A.CBL"))
    )

    @Test
    fun sq216A() = rewriteRun(
        cobol(getNistSource("SQ216A.CBL"))
    )

    @Test
    fun sq217A() = rewriteRun(
        cobol(getNistSource("SQ217A.CBL"))
    )

    @Test
    fun sq218A() = rewriteRun(
        cobol(getNistSource("SQ218A.CBL"))
    )

    @Test
    fun sq219A() = rewriteRun(
        cobol(getNistSource("SQ219A.CBL"))
    )

    @Test
    fun sq220A() = rewriteRun(
        cobol(getNistSource("SQ220A.CBL"))
    )

    @Test
    fun sq221A() = rewriteRun(
        cobol(getNistSource("SQ221A.CBL"))
    )

    @Test
    fun sq222A() = rewriteRun(
        cobol(getNistSource("SQ222A.CBL"))
    )

    @Test
    fun sq223A() = rewriteRun(
        cobol(getNistSource("SQ223A.CBL"))
    )

    @Test
    fun sq224A() = rewriteRun(
        cobol(getNistSource("SQ224A.CBL"))
    )

    @Test
    fun sq225A() = rewriteRun(
        cobol(getNistSource("SQ225A.CBL"))
    )

    @Test
    fun sq226A() = rewriteRun(
        cobol(getNistSource("SQ226A.CBL"))
    )

    @Test
    fun sq227A() = rewriteRun(
        cobol(getNistSource("SQ227A.CBL"))
    )

    @Test
    fun sq228A() = rewriteRun(
        cobol(getNistSource("SQ228A.CBL"))
    )

    @Test
    fun sq229A() = rewriteRun(
        cobol(getNistSource("SQ229A.CBL"))
    )

    @Test
    fun sq230A() = rewriteRun(
        cobol(getNistSource("SQ230A.CBL"))
    )

    @Test
    fun sq302M() = rewriteRun(
        cobol(getNistSource("SQ302M.CBL"))
    )

    @Test
    fun sq303M() = rewriteRun(
        cobol(getNistSource("SQ303M.CBL"))
    )

    @Test
    fun sq401M() = rewriteRun(
        cobol(getNistSource("SQ401M.CBL"))
    )

    @Test
    fun st101A() = rewriteRun(
        cobol(getNistSource("st101A.CBL"))
    )

    @Test
    fun st102A() = rewriteRun(
        cobol(getNistSource("st102A.CBL"))
    )

    @Test
    fun st103A() = rewriteRun(
        cobol(getNistSource("st103A.CBL"))
    )

    @Test
    fun st104A() = rewriteRun(
        cobol(getNistSource("st104A.CBL"))
    )

    @Test
    fun st105A() = rewriteRun(
        cobol(getNistSource("st105A.CBL"))
    )

    @Test
    fun st106A() = rewriteRun(
        cobol(getNistSource("st106A.CBL"))
    )

    @Test
    fun st107A() = rewriteRun(
        cobol(getNistSource("st107A.CBL"))
    )

    @Test
    fun st108A() = rewriteRun(
        cobol(getNistSource("st108A.CBL"))
    )

    @Test
    fun st109A() = rewriteRun(
        cobol(getNistSource("st109A.CBL"))
    )

    @Test
    fun st110A() = rewriteRun(
        cobol(getNistSource("st110A.CBL"))
    )

    @Test
    fun st111A() = rewriteRun(
        cobol(getNistSource("st111A.CBL"))
    )

    @Test
    fun st112M() = rewriteRun(
        cobol(getNistSource("st112M.CBL"))
    )

    @Test
    fun st113M() = rewriteRun(
        cobol(getNistSource("st113M.CBL"))
    )

    @Test
    fun st114M() = rewriteRun(
        cobol(getNistSource("st114M.CBL"))
    )

    @Test
    fun st115A() = rewriteRun(
        cobol(getNistSource("st115A.CBL"))
    )

    @Test
    fun st116A() = rewriteRun(
        cobol(getNistSource("st116A.CBL"))
    )

    @Test
    fun st117A() = rewriteRun(
        cobol(getNistSource("st117A.CBL"))
    )

    @Test
    fun st118A() = rewriteRun(
        cobol(getNistSource("st118A.CBL"))
    )

    @Test
    fun st119A() = rewriteRun(
        cobol(getNistSource("st119A.CBL"))
    )

    @Test
    fun st120A() = rewriteRun(
        cobol(getNistSource("st120A.CBL"))
    )

    @Test
    fun st121A() = rewriteRun(
        cobol(getNistSource("st121A.CBL"))
    )

    @Test
    fun st122A() = rewriteRun(
        cobol(getNistSource("st122A.CBL"))
    )

    @Test
    fun st123A() = rewriteRun(
        cobol(getNistSource("st123A.CBL"))
    )

    @Test
    fun st124A() = rewriteRun(
        cobol(getNistSource("st124A.CBL"))
    )

    @Test
    fun st125A() = rewriteRun(
        cobol(getNistSource("st125A.CBL"))
    )

    @Test
    fun st126A() = rewriteRun(
        cobol(getNistSource("st126A.CBL"))
    )

    @Test
    fun st127A() = rewriteRun(
        cobol(getNistSource("st127A.CBL"))
    )

    @Test
    fun st131A() = rewriteRun(
        cobol(getNistSource("st131A.CBL"))
    )

    @Test
    fun st132A() = rewriteRun(
        cobol(getNistSource("st132A.CBL"))
    )

    @Test
    fun st133A() = rewriteRun(
        cobol(getNistSource("st133A.CBL"))
    )

    @Test
    fun st134A() = rewriteRun(
        cobol(getNistSource("st134A.CBL"))
    )

    @Test
    fun st135A() = rewriteRun(
        cobol(getNistSource("st135A.CBL"))
    )

    @Test
    fun st136A() = rewriteRun(
        cobol(getNistSource("st136A.CBL"))
    )

    @Test
    fun st137A() = rewriteRun(
        cobol(getNistSource("st137A.CBL"))
    )

    @Test
    fun st139A() = rewriteRun(
        cobol(getNistSource("st139A.CBL"))
    )

    @Test
    fun st140A() = rewriteRun(
        cobol(getNistSource("st140A.CBL"))
    )

    @Test
    fun st144A() = rewriteRun(
        cobol(getNistSource("st144A.CBL"))
    )

    @Test
    fun st146A() = rewriteRun(
        cobol(getNistSource("st146A.CBL"))
    )

    @Test
    fun st147A() = rewriteRun(
        cobol(getNistSource("st147A.CBL"))
    )

    @Test
    fun st301M() = rewriteRun(
        cobol(getNistSource("st301M.CBL"))
    )
}
