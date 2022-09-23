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

import org.junit.jupiter.api.Test
import org.openrewrite.ExecutionContext
import org.openrewrite.cobol.Assertions.cobolPreprocess
import org.openrewrite.cobol.CobolPreprocessorParser
import org.openrewrite.cobol.CobolPreprocessorVisitor
import org.openrewrite.internal.EncodingDetectingInputStream
import org.openrewrite.test.RecipeSpec
import org.openrewrite.test.RewriteTest
import java.nio.file.Files
import java.nio.file.Paths

class CobolPreprocessorNistTest : RewriteTest {

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
        spec.parser(CobolPreprocessorParser.builder().enableCopy())
            .recipe(RewriteTest.toRecipe {
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
            }).parser(CobolPreprocessorParser.builder())
    }

    @Test
    fun cm101M() = rewriteRun(
        cobolPreprocess(getNistSource("CM101M.CBL"))
    )

    @Test
    fun cm102M() = rewriteRun(
        cobolPreprocess(getNistSource("CM102M.CBL"))
    )

    @Test
    fun cm103M() = rewriteRun(
        cobolPreprocess(getNistSource("CM103M.CBL"))
    )

    @Test
    fun cm104M() = rewriteRun(
        cobolPreprocess(getNistSource("CM104M.CBL"))
    )

    @Test
    fun cm105M() = rewriteRun(
        cobolPreprocess(getNistSource("CM105M.CBL"))
    )

    @Test
    fun cm201M() = rewriteRun(
        cobolPreprocess(getNistSource("CM201M.CBL"))
    )

    @Test
    fun cm202M() = rewriteRun(
        cobolPreprocess(getNistSource("CM202M.CBL"))
    )

    @Test
    fun cm303M() = rewriteRun(
        cobolPreprocess(getNistSource("CM303M.CBL"))
    )

    @Test
    fun cm401M() = rewriteRun(
        cobolPreprocess(getNistSource("CM401M.CBL"))
    )

    @Test
    fun db101A() = rewriteRun(
        cobolPreprocess(getNistSource("DB101A.CBL"))
    )

    @Test
    fun db102A() = rewriteRun(
        cobolPreprocess(getNistSource("DB102A.CBL"))
    )

    @Test
    fun db103M() = rewriteRun(
        cobolPreprocess(getNistSource("DB103M.CBL"))
    )

    @Test
    fun db104A() = rewriteRun(
        cobolPreprocess(getNistSource("DB104A.CBL"))
    )

    @Test
    fun db105A() = rewriteRun(
        cobolPreprocess(getNistSource("DB105A.CBL"))
    )

    @Test
    fun db201A() = rewriteRun(
        cobolPreprocess(getNistSource("DB201A.CBL"))
    )

    @Test
    fun db202A() = rewriteRun(
        cobolPreprocess(getNistSource("DB202A.CBL"))
    )

    @Test
    fun db203A() = rewriteRun(
        cobolPreprocess(getNistSource("DB203A.CBL"))
    )

    @Test
    fun db204A() = rewriteRun(
        cobolPreprocess(getNistSource("DB204A.CBL"))
    )

    @Test
    fun db205A() = rewriteRun(
        cobolPreprocess(getNistSource("DB205A.CBL"))
    )

    @Test
    fun db301M() = rewriteRun(
        cobolPreprocess(getNistSource("DB301M.CBL"))
    )

    @Test
    fun db302M() = rewriteRun(
        cobolPreprocess(getNistSource("DB302M.CBL"))
    )

    @Test
    fun db303M() = rewriteRun(
        cobolPreprocess(getNistSource("DB303M.CBL"))
    )

    @Test
    fun db304M() = rewriteRun(
        cobolPreprocess(getNistSource("DB304M.CBL"))
    )

    @Test
    fun db305M() = rewriteRun(
        cobolPreprocess(getNistSource("DB305M.CBL"))
    )

    @Test
    fun exec85() = rewriteRun(
        cobolPreprocess(getNistSource("EXEC85.CBL"))
    )

    @Test
    fun ic101A() = rewriteRun(
        cobolPreprocess(getNistSource("IC101A.CBL"))
    )

    @Test
    fun ic102A() = rewriteRun(
        cobolPreprocess(getNistSource("IC102A.CBL"))
    )

    @Test
    fun ic103A() = rewriteRun(
        cobolPreprocess(getNistSource("IC103A.CBL"))
    )

    @Test
    fun ic104A() = rewriteRun(
        cobolPreprocess(getNistSource("IC104A.CBL"))
    )

    @Test
    fun ic105A() = rewriteRun(
        cobolPreprocess(getNistSource("IC105A.CBL"))
    )

    @Test
    fun ic106A() = rewriteRun(
        cobolPreprocess(getNistSource("IC106A.CBL"))
    )

    @Test
    fun ic107A() = rewriteRun(
        cobolPreprocess(getNistSource("IC107A.CBL"))
    )

    @Test
    fun ic108A() = rewriteRun(
        cobolPreprocess(getNistSource("IC108A.CBL"))
    )

    @Test
    fun ic109A() = rewriteRun(
        cobolPreprocess(getNistSource("IC109A.CBL"))
    )

    @Test
    fun ic110A() = rewriteRun(
        cobolPreprocess(getNistSource("IC110A.CBL"))
    )

    @Test
    fun ic111A() = rewriteRun(
        cobolPreprocess(getNistSource("IC111A.CBL"))
    )

    @Test
    fun ic112A() = rewriteRun(
        cobolPreprocess(getNistSource("IC112A.CBL"))
    )

    @Test
    fun ic113A() = rewriteRun(
        cobolPreprocess(getNistSource("IC113A.CBL"))
    )

    @Test
    fun ic114A() = rewriteRun(
        cobolPreprocess(getNistSource("IC114A.CBL"))
    )

    @Test
    fun ic115A() = rewriteRun(
        cobolPreprocess(getNistSource("IC115A.CBL"))
    )

    @Test
    fun ic116M() = rewriteRun(
        cobolPreprocess(getNistSource("IC116M.CBL"))
    )

    @Test
    fun ic117M() = rewriteRun(
        cobolPreprocess(getNistSource("IC117M.CBL"))
    )

    @Test
    fun ic118M() = rewriteRun(
        cobolPreprocess(getNistSource("IC118M.CBL"))
    )

    @Test
    fun ic201A() = rewriteRun(
        cobolPreprocess(getNistSource("IC201A.CBL"))
    )

    @Test
    fun ic202A() = rewriteRun(
        cobolPreprocess(getNistSource("IC202A.CBL"))
    )

    @Test
    fun ic203A() = rewriteRun(
        cobolPreprocess(getNistSource("IC203A.CBL"))
    )

    @Test
    fun ic204A() = rewriteRun(
        cobolPreprocess(getNistSource("IC204A.CBL"))
    )

    @Test
    fun ic205A() = rewriteRun(
        cobolPreprocess(getNistSource("IC205A.CBL"))
    )

    @Test
    fun ic206A() = rewriteRun(
        cobolPreprocess(getNistSource("IC206A.CBL"))
    )

    @Test
    fun ic207A() = rewriteRun(
        cobolPreprocess(getNistSource("IC207A.CBL"))
    )

    @Test
    fun ic208A() = rewriteRun(
        cobolPreprocess(getNistSource("IC208A.CBL"))
    )

    @Test
    fun ic209A() = rewriteRun(
        cobolPreprocess(getNistSource("IC209A.CBL"))
    )

    @Test
    fun ic210A() = rewriteRun(
        cobolPreprocess(getNistSource("IC210A.CBL"))
    )

    @Test
    fun ic211A() = rewriteRun(
        cobolPreprocess(getNistSource("IC211A.CBL"))
    )

    @Test
    fun ic212A() = rewriteRun(
        cobolPreprocess(getNistSource("IC212A.CBL"))
    )

    @Test
    fun ic213A() = rewriteRun(
        cobolPreprocess(getNistSource("IC213A.CBL"))
    )

    @Test
    fun ic214A() = rewriteRun(
        cobolPreprocess(getNistSource("IC214A.CBL"))
    )

    @Test
    fun ic215A() = rewriteRun(
        cobolPreprocess(getNistSource("IC215A.CBL"))
    )

    @Test
    fun ic216A() = rewriteRun(
        cobolPreprocess(getNistSource("IC216A.CBL"))
    )

    @Test
    fun ic217A() = rewriteRun(
        cobolPreprocess(getNistSource("IC217A.CBL"))
    )

    @Test
    fun ic222A() = rewriteRun(
        cobolPreprocess(getNistSource("IC222A.CBL"))
    )

    @Test
    fun ic223A() = rewriteRun(
        cobolPreprocess(getNistSource("IC223A.CBL"))
    )

    @Test
    fun ic224A() = rewriteRun(
        cobolPreprocess(getNistSource("IC224A.CBL"))
    )

    @Test
    fun ic225A() = rewriteRun(
        cobolPreprocess(getNistSource("IC225A.CBL"))
    )

    @Test
    fun ic226A() = rewriteRun(
        cobolPreprocess(getNistSource("IC226A.CBL"))
    )

    @Test
    fun ic227A() = rewriteRun(
        cobolPreprocess(getNistSource("IC227A.CBL"))
    )

    @Test
    fun ic228A() = rewriteRun(
        cobolPreprocess(getNistSource("IC228A.CBL"))
    )

    @Test
    fun ic233A() = rewriteRun(
        cobolPreprocess(getNistSource("IC233A.CBL"))
    )

    @Test
    fun ic234A() = rewriteRun(
        cobolPreprocess(getNistSource("IC234A.CBL"))
    )

    @Test
    fun ic235A() = rewriteRun(
        cobolPreprocess(getNistSource("IC235A.CBL"))
    )

    @Test
    fun ic237A() = rewriteRun(
        cobolPreprocess(getNistSource("IC237A.CBL"))
    )

    @Test
    fun ic401M() = rewriteRun(
        cobolPreprocess(getNistSource("IC401M.CBL"))
    )

    @Test
    fun if101A() = rewriteRun(
        cobolPreprocess(getNistSource("IF101A.CBL"))
    )

    @Test
    fun if102A() = rewriteRun(
        cobolPreprocess(getNistSource("IF102A.CBL"))
    )

    @Test
    fun if103A() = rewriteRun(
        cobolPreprocess(getNistSource("IF103A.CBL"))
    )

    @Test
    fun if104A() = rewriteRun(
        cobolPreprocess(getNistSource("IF104A.CBL"))
    )

    @Test
    fun if105A() = rewriteRun(
        cobolPreprocess(getNistSource("IF105A.CBL"))
    )

    @Test
    fun if106A() = rewriteRun(
        cobolPreprocess(getNistSource("IF106A.CBL"))
    )

    @Test
    fun if107A() = rewriteRun(
        cobolPreprocess(getNistSource("IF107A.CBL"))
    )

    @Test
    fun if108A() = rewriteRun(
        cobolPreprocess(getNistSource("IF108A.CBL"))
    )

    @Test
    fun if109A() = rewriteRun(
        cobolPreprocess(getNistSource("IF109A.CBL"))
    )

    @Test
    fun if110A() = rewriteRun(
        cobolPreprocess(getNistSource("IF110A.CBL"))
    )

    @Test
    fun if111A() = rewriteRun(
        cobolPreprocess(getNistSource("IF111A.CBL"))
    )

    @Test
    fun if112A() = rewriteRun(
        cobolPreprocess(getNistSource("IF112A.CBL"))
    )

    @Test
    fun if113A() = rewriteRun(
        cobolPreprocess(getNistSource("IF113A.CBL"))
    )

    @Test
    fun if114A() = rewriteRun(
        cobolPreprocess(getNistSource("IF114A.CBL"))
    )

    @Test
    fun if115A() = rewriteRun(
        cobolPreprocess(getNistSource("IF115A.CBL"))
    )

    @Test
    fun if116A() = rewriteRun(
        cobolPreprocess(getNistSource("IF116A.CBL"))
    )

    @Test
    fun if117A() = rewriteRun(
        cobolPreprocess(getNistSource("IF117A.CBL"))
    )

    @Test
    fun if118A() = rewriteRun(
        cobolPreprocess(getNistSource("IF118A.CBL"))
    )

    @Test
    fun if119A() = rewriteRun(
        cobolPreprocess(getNistSource("IF119A.CBL"))
    )

    @Test
    fun if120A() = rewriteRun(
        cobolPreprocess(getNistSource("IF120A.CBL"))
    )

    @Test
    fun if121A() = rewriteRun(
        cobolPreprocess(getNistSource("IF121A.CBL"))
    )

    @Test
    fun if122A() = rewriteRun(
        cobolPreprocess(getNistSource("IF122A.CBL"))
    )

    @Test
    fun if123A() = rewriteRun(
        cobolPreprocess(getNistSource("IF123A.CBL"))
    )

    @Test
    fun if124A() = rewriteRun(
        cobolPreprocess(getNistSource("IF124A.CBL"))
    )

    @Test
    fun if125A() = rewriteRun(
        cobolPreprocess(getNistSource("IF125A.CBL"))
    )

    @Test
    fun if126A() = rewriteRun(
        cobolPreprocess(getNistSource("IF126A.CBL"))
    )

    @Test
    fun if127A() = rewriteRun(
        cobolPreprocess(getNistSource("IF127A.CBL"))
    )

    @Test
    fun if128A() = rewriteRun(
        cobolPreprocess(getNistSource("IF128A.CBL"))
    )

    @Test
    fun if129A() = rewriteRun(
        cobolPreprocess(getNistSource("IF129A.CBL"))
    )

    @Test
    fun if130A() = rewriteRun(
        cobolPreprocess(getNistSource("IF130A.CBL"))
    )

    @Test
    fun if131A() = rewriteRun(
        cobolPreprocess(getNistSource("IF131A.CBL"))
    )

    @Test
    fun if132A() = rewriteRun(
        cobolPreprocess(getNistSource("IF132A.CBL"))
    )

    @Test
    fun if133A() = rewriteRun(
        cobolPreprocess(getNistSource("IF133A.CBL"))
    )

    @Test
    fun if134A() = rewriteRun(
        cobolPreprocess(getNistSource("IF134A.CBL"))
    )

    @Test
    fun if135A() = rewriteRun(
        cobolPreprocess(getNistSource("IF135A.CBL"))
    )

    @Test
    fun if136A() = rewriteRun(
        cobolPreprocess(getNistSource("IF136A.CBL"))
    )

    @Test
    fun if137A() = rewriteRun(
        cobolPreprocess(getNistSource("IF137A.CBL"))
    )

    @Test
    fun if138A() = rewriteRun(
        cobolPreprocess(getNistSource("IF138A.CBL"))
    )

    @Test
    fun if139A() = rewriteRun(
        cobolPreprocess(getNistSource("IF139A.CBL"))
    )

    @Test
    fun if140A() = rewriteRun(
        cobolPreprocess(getNistSource("IF140A.CBL"))
    )

    @Test
    fun if141A() = rewriteRun(
        cobolPreprocess(getNistSource("IF141A.CBL"))
    )

    @Test
    fun if142A() = rewriteRun(
        cobolPreprocess(getNistSource("IF142A.CBL"))
    )

    @Test
    fun if401M() = rewriteRun(
        cobolPreprocess(getNistSource("IF401M.CBL"))
    )

    @Test
    fun if402M() = rewriteRun(
        cobolPreprocess(getNistSource("IF402M.CBL"))
    )

    @Test
    fun if403M() = rewriteRun(
        cobolPreprocess(getNistSource("IF403M.CBL"))
    )

    @Test
    fun ix101A() = rewriteRun(
        cobolPreprocess(getNistSource("IX101A.CBL"))
    )

    @Test
    fun ix102A() = rewriteRun(
        cobolPreprocess(getNistSource("IX102A.CBL"))
    )

    @Test
    fun ix103A() = rewriteRun(
        cobolPreprocess(getNistSource("IX103A.CBL"))
    )

    @Test
    fun ix104A() = rewriteRun(
        cobolPreprocess(getNistSource("IX104A.CBL"))
    )

    @Test
    fun ix105A() = rewriteRun(
        cobolPreprocess(getNistSource("IX105A.CBL"))
    )

    @Test
    fun ix106A() = rewriteRun(
        cobolPreprocess(getNistSource("IX106A.CBL"))
    )

    @Test
    fun ix107A() = rewriteRun(
        cobolPreprocess(getNistSource("IX107A.CBL"))
    )

    @Test
    fun ix108A() = rewriteRun(
        cobolPreprocess(getNistSource("IX108A.CBL"))
    )

    @Test
    fun ix109A() = rewriteRun(
        cobolPreprocess(getNistSource("IX109A.CBL"))
    )

    @Test
    fun ix110A() = rewriteRun(
        cobolPreprocess(getNistSource("IX110A.CBL"))
    )

    @Test
    fun ix111A() = rewriteRun(
        cobolPreprocess(getNistSource("IX111A.CBL"))
    )

    @Test
    fun ix112A() = rewriteRun(
        cobolPreprocess(getNistSource("IX112A.CBL"))
    )

    @Test
    fun ix113A() = rewriteRun(
        cobolPreprocess(getNistSource("IX113A.CBL"))
    )

    @Test
    fun ix114A() = rewriteRun(
        cobolPreprocess(getNistSource("IX114A.CBL"))
    )

    @Test
    fun ix115A() = rewriteRun(
        cobolPreprocess(getNistSource("IX115A.CBL"))
    )

    @Test
    fun ix116A() = rewriteRun(
        cobolPreprocess(getNistSource("IX116A.CBL"))
    )

    @Test
    fun ix117A() = rewriteRun(
        cobolPreprocess(getNistSource("IX117A.CBL"))
    )

    @Test
    fun ix118A() = rewriteRun(
        cobolPreprocess(getNistSource("IX118A.CBL"))
    )

    @Test
    fun ix119A() = rewriteRun(
        cobolPreprocess(getNistSource("IX119A.CBL"))
    )

    @Test
    fun ix120A() = rewriteRun(
        cobolPreprocess(getNistSource("IX120A.CBL"))
    )

    @Test
    fun ix121A() = rewriteRun(
        cobolPreprocess(getNistSource("IX121A.CBL"))
    )

    @Test
    fun ix201A() = rewriteRun(
        cobolPreprocess(getNistSource("IX201A.CBL"))
    )

    @Test
    fun ix202A() = rewriteRun(
        cobolPreprocess(getNistSource("IX202A.CBL"))
    )

    @Test
    fun ix203A() = rewriteRun(
        cobolPreprocess(getNistSource("IX203A.CBL"))
    )

    @Test
    fun ix204A() = rewriteRun(
        cobolPreprocess(getNistSource("IX204A.CBL"))
    )

    @Test
    fun ix205A() = rewriteRun(
        cobolPreprocess(getNistSource("IX205A.CBL"))
    )

    @Test
    fun ix206A() = rewriteRun(
        cobolPreprocess(getNistSource("IX206A.CBL"))
    )

    @Test
    fun ix207A() = rewriteRun(
        cobolPreprocess(getNistSource("IX207A.CBL"))
    )

    @Test
    fun ix208A() = rewriteRun(
        cobolPreprocess(getNistSource("IX208A.CBL"))
    )

    @Test
    fun ix209A() = rewriteRun(
        cobolPreprocess(getNistSource("IX209A.CBL"))
    )

    @Test
    fun ix210A() = rewriteRun(
        cobolPreprocess(getNistSource("IX210A.CBL"))
    )

    @Test
    fun ix211A() = rewriteRun(
        cobolPreprocess(getNistSource("IX211A.CBL"))
    )

    @Test
    fun ix212A() = rewriteRun(
        cobolPreprocess(getNistSource("IX212A.CBL"))
    )

    @Test
    fun ix213A() = rewriteRun(
        cobolPreprocess(getNistSource("IX213A.CBL"))
    )

    @Test
    fun ix214A() = rewriteRun(
        cobolPreprocess(getNistSource("IX214A.CBL"))
    )

    @Test
    fun ix215A() = rewriteRun(
        cobolPreprocess(getNistSource("IX215A.CBL"))
    )

    @Test
    fun ix216A() = rewriteRun(
        cobolPreprocess(getNistSource("IX216A.CBL"))
    )

    @Test
    fun ix217A() = rewriteRun(
        cobolPreprocess(getNistSource("IX217A.CBL"))
    )

    @Test
    fun ix218A() = rewriteRun(
        cobolPreprocess(getNistSource("IX218A.CBL"))
    )

    @Test
    fun ix301M() = rewriteRun(
        cobolPreprocess(getNistSource("IX301M.CBL"))
    )

    @Test
    fun ix302M() = rewriteRun(
        cobolPreprocess(getNistSource("IX302M.CBL"))
    )

    @Test
    fun ix401M() = rewriteRun(
        cobolPreprocess(getNistSource("IX401M.CBL"))
    )

    @Test
    fun nc101A() = rewriteRun(
        cobolPreprocess(getNistSource("NC101A.CBL"))
    )

    @Test
    fun nc102A() = rewriteRun(
        cobolPreprocess(getNistSource("NC102A.CBL"))
    )

    @Test
    fun nc103A() = rewriteRun(
        cobolPreprocess(getNistSource("NC103A.CBL"))
    )

    @Test
    fun nc104A() = rewriteRun(
        cobolPreprocess(getNistSource("NC104A.CBL"))
    )

    @Test
    fun nc105A() = rewriteRun(
        cobolPreprocess(getNistSource("NC105A.CBL"))
    )

    @Test
    fun nc106A() = rewriteRun(
        cobolPreprocess(getNistSource("NC106A.CBL"))
    )

    @Test
    fun nc107A() = rewriteRun(
        cobolPreprocess(getNistSource("NC107A.CBL"))
    )

    @Test
    fun nc108M() = rewriteRun(
        cobolPreprocess(getNistSource("NC108M.CBL"))
    )

    @Test
    fun nc109M() = rewriteRun(
        cobolPreprocess(getNistSource("NC109M.CBL"))
    )

    @Test
    fun nc110M() = rewriteRun(
        cobolPreprocess(getNistSource("NC110M.CBL"))
    )

    @Test
    fun nc111A() = rewriteRun(
        cobolPreprocess(getNistSource("NC111A.CBL"))
    )

    @Test
    fun nc112A() = rewriteRun(
        cobolPreprocess(getNistSource("NC112A.CBL"))
    )

    @Test
    fun nc113M() = rewriteRun(
        cobolPreprocess(getNistSource("NC113M.CBL"))
    )

    @Test
    fun nc114M() = rewriteRun(
        cobolPreprocess(getNistSource("NC114M.CBL"))
    )

    @Test
    fun nc115A() = rewriteRun(
        cobolPreprocess(getNistSource("NC115A.CBL"))
    )

    @Test
    fun nc116A() = rewriteRun(
        cobolPreprocess(getNistSource("NC116A.CBL"))
    )

    @Test
    fun nc117A() = rewriteRun(
        cobolPreprocess(getNistSource("NC117A.CBL"))
    )

    @Test
    fun nc118A() = rewriteRun(
        cobolPreprocess(getNistSource("NC118A.CBL"))
    )

    @Test
    fun nc119A() = rewriteRun(
        cobolPreprocess(getNistSource("NC119A.CBL"))
    )

    @Test
    fun nc120A() = rewriteRun(
        cobolPreprocess(getNistSource("NC120A.CBL"))
    )

    @Test
    fun nc121M() = rewriteRun(
        cobolPreprocess(getNistSource("NC121M.CBL"))
    )

    @Test
    fun nc122A() = rewriteRun(
        cobolPreprocess(getNistSource("NC122A.CBL"))
    )

    @Test
    fun nc123A() = rewriteRun(
        cobolPreprocess(getNistSource("NC123A.CBL"))
    )

    @Test
    fun nc124A() = rewriteRun(
        cobolPreprocess(getNistSource("NC124A.CBL"))
    )

    @Test
    fun nc125A() = rewriteRun(
        cobolPreprocess(getNistSource("NC125A.CBL"))
    )

    @Test
    fun nc126A() = rewriteRun(
        cobolPreprocess(getNistSource("NC126A.CBL"))
    )

    @Test
    fun nc127A() = rewriteRun(
        cobolPreprocess(getNistSource("NC127A.CBL"))
    )

    @Test
    fun nc131A() = rewriteRun(
        cobolPreprocess(getNistSource("NC131A.CBL"))
    )

    @Test
    fun nc132A() = rewriteRun(
        cobolPreprocess(getNistSource("NC132A.CBL"))
    )

    @Test
    fun nc133A() = rewriteRun(
        cobolPreprocess(getNistSource("NC133A.CBL"))
    )

    @Test
    fun nc134A() = rewriteRun(
        cobolPreprocess(getNistSource("NC134A.CBL"))
    )

    @Test
    fun nc135A() = rewriteRun(
        cobolPreprocess(getNistSource("NC135A.CBL"))
    )

    @Test
    fun nc136A() = rewriteRun(
        cobolPreprocess(getNistSource("NC136A.CBL"))
    )

    @Test
    fun nc137A() = rewriteRun(
        cobolPreprocess(getNistSource("NC137A.CBL"))
    )

    @Test
    fun nc138A() = rewriteRun(
        cobolPreprocess(getNistSource("NC138A.CBL"))
    )

    @Test
    fun nc139A() = rewriteRun(
        cobolPreprocess(getNistSource("NC139A.CBL"))
    )

    @Test
    fun nc140A() = rewriteRun(
        cobolPreprocess(getNistSource("NC140A.CBL"))
    )

    @Test
    fun nc141A() = rewriteRun(
        cobolPreprocess(getNistSource("NC141A.CBL"))
    )

    @Test
    fun nc171A() = rewriteRun(
        cobolPreprocess(getNistSource("NC171A.CBL"))
    )

    @Test
    fun nc172A() = rewriteRun(
        cobolPreprocess(getNistSource("NC172A.CBL"))
    )

    @Test
    fun nc173A() = rewriteRun(
        cobolPreprocess(getNistSource("NC173A.CBL"))
    )

    @Test
    fun nc174A() = rewriteRun(
        cobolPreprocess(getNistSource("NC174A.CBL"))
    )

    @Test
    fun nc175A() = rewriteRun(
        cobolPreprocess(getNistSource("NC175A.CBL"))
    )

    @Test
    fun nc176A() = rewriteRun(
        cobolPreprocess(getNistSource("NC176A.CBL"))
    )

    @Test
    fun nc177A() = rewriteRun(
        cobolPreprocess(getNistSource("NC177A.CBL"))
    )

    @Test
    fun nc201A() = rewriteRun(
        cobolPreprocess(getNistSource("NC201A.CBL"))
    )

    @Test
    fun nc202A() = rewriteRun(
        cobolPreprocess(getNistSource("NC202A.CBL"))
    )

    @Test
    fun nc203A() = rewriteRun(
        cobolPreprocess(getNistSource("NC203A.CBL"))
    )

    @Test
    fun nc204M() = rewriteRun(
        cobolPreprocess(getNistSource("NC204M.CBL"))
    )

    @Test
    fun nc205A() = rewriteRun(
        cobolPreprocess(getNistSource("NC205A.CBL"))
    )

    @Test
    fun nc206A() = rewriteRun(
        cobolPreprocess(getNistSource("NC206A.CBL"))
    )

    @Test
    fun nc207A() = rewriteRun(
        cobolPreprocess(getNistSource("NC207A.CBL"))
    )

    @Test
    fun nc208A() = rewriteRun(
        cobolPreprocess(getNistSource("NC208A.CBL"))
    )

    @Test
    fun nc209A() = rewriteRun(
        cobolPreprocess(getNistSource("NC209A.CBL"))
    )

    @Test
    fun nc210A() = rewriteRun(
        cobolPreprocess(getNistSource("NC210A.CBL"))
    )

    @Test
    fun nc211A() = rewriteRun(
        cobolPreprocess(getNistSource("NC211A.CBL"))
    )

    @Test
    fun nc214M() = rewriteRun(
        cobolPreprocess(getNistSource("NC214M.CBL"))
    )

    @Test
    fun nc215A() = rewriteRun(
        cobolPreprocess(getNistSource("NC215A.CBL"))
    )

    @Test
    fun nc216A() = rewriteRun(
        cobolPreprocess(getNistSource("NC216A.CBL"))
    )

    @Test
    fun nc217A() = rewriteRun(
        cobolPreprocess(getNistSource("NC217A.CBL"))
    )

    @Test
    fun nc218A() = rewriteRun(
        cobolPreprocess(getNistSource("NC218A.CBL"))
    )

    @Test
    fun nc219A() = rewriteRun(
        cobolPreprocess(getNistSource("NC219A.CBL"))
    )

    @Test
    fun nc220M() = rewriteRun(
        cobolPreprocess(getNistSource("NC220M.CBL"))
    )

    @Test
    fun nc221A() = rewriteRun(
        cobolPreprocess(getNistSource("NC221A.CBL"))
    )

    @Test
    fun nc222A() = rewriteRun(
        cobolPreprocess(getNistSource("NC222A.CBL"))
    )

    @Test
    fun nc223A() = rewriteRun(
        cobolPreprocess(getNistSource("NC223A.CBL"))
    )

    @Test
    fun nc224A() = rewriteRun(
        cobolPreprocess(getNistSource("NC224A.CBL"))
    )

    @Test
    fun nc225A() = rewriteRun(
        cobolPreprocess(getNistSource("NC225A.CBL"))
    )

    @Test
    fun nc231A() = rewriteRun(
        cobolPreprocess(getNistSource("NC231A.CBL"))
    )

    @Test
    fun nc232A() = rewriteRun(
        cobolPreprocess(getNistSource("NC232A.CBL"))
    )

    @Test
    fun nc233A() = rewriteRun(
        cobolPreprocess(getNistSource("NC233A.CBL"))
    )

    @Test
    fun nc234A() = rewriteRun(
        cobolPreprocess(getNistSource("NC234A.CBL"))
    )

    @Test
    fun nc235A() = rewriteRun(
        cobolPreprocess(getNistSource("NC235A.CBL"))
    )

    @Test
    fun nc236A() = rewriteRun(
        cobolPreprocess(getNistSource("NC236A.CBL"))
    )

    @Test
    fun nc237A() = rewriteRun(
        cobolPreprocess(getNistSource("NC237A.CBL"))
    )

    @Test
    fun nc238A() = rewriteRun(
        cobolPreprocess(getNistSource("NC238A.CBL"))
    )

    @Test
    fun nc239A() = rewriteRun(
        cobolPreprocess(getNistSource("NC239A.CBL"))
    )

    @Test
    fun nc240A() = rewriteRun(
        cobolPreprocess(getNistSource("NC240A.CBL"))
    )

    @Test
    fun nc241A() = rewriteRun(
        cobolPreprocess(getNistSource("NC241A.CBL"))
    )

    @Test
    fun nc242A() = rewriteRun(
        cobolPreprocess(getNistSource("NC242A.CBL"))
    )

    @Test
    fun nc243A() = rewriteRun(
        cobolPreprocess(getNistSource("NC243A.CBL"))
    )

    @Test
    fun nc244A() = rewriteRun(
        cobolPreprocess(getNistSource("NC244A.CBL"))
    )

    @Test
    fun nc245A() = rewriteRun(
        cobolPreprocess(getNistSource("NC245A.CBL"))
    )

    @Test
    fun nc246A() = rewriteRun(
        cobolPreprocess(getNistSource("NC246A.CBL"))
    )

    @Test
    fun nc247A() = rewriteRun(
        cobolPreprocess(getNistSource("NC247A.CBL"))
    )

    @Test
    fun nc248A() = rewriteRun(
        cobolPreprocess(getNistSource("NC248A.CBL"))
    )

    @Test
    fun nc250A() = rewriteRun(
        cobolPreprocess(getNistSource("NC250A.CBL"))
    )

    @Test
    fun nc251A() = rewriteRun(
        cobolPreprocess(getNistSource("NC251A.CBL"))
    )

    @Test
    fun nc252A() = rewriteRun(
        cobolPreprocess(getNistSource("NC252A.CBL"))
    )

    @Test
    fun nc253A() = rewriteRun(
        cobolPreprocess(getNistSource("NC253A.CBL"))
    )

    @Test
    fun nc254A() = rewriteRun(
        cobolPreprocess(getNistSource("NC254A.CBL"))
    )

    @Test
    fun nc302M() = rewriteRun(
        cobolPreprocess(getNistSource("NC302M.CBL"))
    )

    @Test
    fun nc303M() = rewriteRun(
        cobolPreprocess(getNistSource("NC303M.CBL"))
    )

    @Test
    fun nc401M() = rewriteRun(
        cobolPreprocess(getNistSource("NC401M.CBL"))
    )

    @Test
    fun obic1A() = rewriteRun(
        cobolPreprocess(getNistSource("OBIC1A.CBL"))
    )

    @Test
    fun obic2A() = rewriteRun(
        cobolPreprocess(getNistSource("OBIC2A.CBL"))
    )

    @Test
    fun obic3A() = rewriteRun(
        cobolPreprocess(getNistSource("OBIC3A.CBL"))
    )

    @Test
    fun obnc1M() = rewriteRun(
        cobolPreprocess(getNistSource("OBNC1M.CBL"))
    )

    @Test
    fun obnc2M() = rewriteRun(
        cobolPreprocess(getNistSource("OBNC2M.CBL"))
    )

    @Test
    fun obsq1A() = rewriteRun(
        cobolPreprocess(getNistSource("OBSQ1A.CBL"))
    )

    @Test
    fun obsq3A() = rewriteRun(
        cobolPreprocess(getNistSource("OBSQ3A.CBL"))
    )

    @Test
    fun obsq4A() = rewriteRun(
        cobolPreprocess(getNistSource("OBSQ4A.CBL"))
    )

    @Test
    fun obsq5A() = rewriteRun(
        cobolPreprocess(getNistSource("OBSQ5A.CBL"))
    )

    @Test
    fun rl101A() = rewriteRun(
        cobolPreprocess(getNistSource("RL101A.CBL"))
    )

    @Test
    fun rl102A() = rewriteRun(
        cobolPreprocess(getNistSource("RL102A.CBL"))
    )

    @Test
    fun rl103A() = rewriteRun(
        cobolPreprocess(getNistSource("RL103A.CBL"))
    )

    @Test
    fun rl104A() = rewriteRun(
        cobolPreprocess(getNistSource("RL104A.CBL"))
    )

    @Test
    fun rl105A() = rewriteRun(
        cobolPreprocess(getNistSource("RL105A.CBL"))
    )

    @Test
    fun rl106A() = rewriteRun(
        cobolPreprocess(getNistSource("RL106A.CBL"))
    )

    @Test
    fun rl107A() = rewriteRun(
        cobolPreprocess(getNistSource("RL107A.CBL"))
    )

    @Test
    fun rl108A() = rewriteRun(
        cobolPreprocess(getNistSource("RL108A.CBL"))
    )

    @Test
    fun rl109A() = rewriteRun(
        cobolPreprocess(getNistSource("RL109A.CBL"))
    )

    @Test
    fun rl110A() = rewriteRun(
        cobolPreprocess(getNistSource("RL110A.CBL"))
    )

    @Test
    fun rl111A() = rewriteRun(
        cobolPreprocess(getNistSource("RL111A.CBL"))
    )

    @Test
    fun rl112A() = rewriteRun(
        cobolPreprocess(getNistSource("RL112A.CBL"))
    )

    @Test
    fun rl113A() = rewriteRun(
        cobolPreprocess(getNistSource("RL113A.CBL"))
    )

    @Test
    fun rl114A() = rewriteRun(
        cobolPreprocess(getNistSource("RL114A.CBL"))
    )

    @Test
    fun rl115A() = rewriteRun(
        cobolPreprocess(getNistSource("RL115A.CBL"))
    )

    @Test
    fun rl116A() = rewriteRun(
        cobolPreprocess(getNistSource("RL116A.CBL"))
    )

    @Test
    fun rl117A() = rewriteRun(
        cobolPreprocess(getNistSource("RL117A.CBL"))
    )

    @Test
    fun rl118A() = rewriteRun(
        cobolPreprocess(getNistSource("RL118A.CBL"))
    )

    @Test
    fun rl119A() = rewriteRun(
        cobolPreprocess(getNistSource("RL119A.CBL"))
    )

    @Test
    fun rl201A() = rewriteRun(
        cobolPreprocess(getNistSource("RL201A.CBL"))
    )

    @Test
    fun rl202A() = rewriteRun(
        cobolPreprocess(getNistSource("RL202A.CBL"))
    )

    @Test
    fun rl203A() = rewriteRun(
        cobolPreprocess(getNistSource("RL203A.CBL"))
    )

    @Test
    fun rl204A() = rewriteRun(
        cobolPreprocess(getNistSource("RL204A.CBL"))
    )

    @Test
    fun rl205A() = rewriteRun(
        cobolPreprocess(getNistSource("RL205A.CBL"))
    )

    @Test
    fun rl206A() = rewriteRun(
        cobolPreprocess(getNistSource("RL206A.CBL"))
    )

    @Test
    fun rl207A() = rewriteRun(
        cobolPreprocess(getNistSource("RL207A.CBL"))
    )

    @Test
    fun rl208A() = rewriteRun(
        cobolPreprocess(getNistSource("RL208A.CBL"))
    )

    @Test
    fun rl209A() = rewriteRun(
        cobolPreprocess(getNistSource("RL209A.CBL"))
    )

    @Test
    fun rl210A() = rewriteRun(
        cobolPreprocess(getNistSource("RL210A.CBL"))
    )

    @Test
    fun rl211A() = rewriteRun(
        cobolPreprocess(getNistSource("RL211A.CBL"))
    )

    @Test
    fun rl212A() = rewriteRun(
        cobolPreprocess(getNistSource("RL212A.CBL"))
    )

    @Test
    fun rl213A() = rewriteRun(
        cobolPreprocess(getNistSource("RL213A.CBL"))
    )

    @Test
    fun rl301M() = rewriteRun(
        cobolPreprocess(getNistSource("RL301M.CBL"))
    )

    @Test
    fun rl302M() = rewriteRun(
        cobolPreprocess(getNistSource("RL302M.CBL"))
    )

    @Test
    fun rl401M() = rewriteRun(
        cobolPreprocess(getNistSource("RL401M.CBL"))
    )

    @Test
    fun rw101A() = rewriteRun(
        cobolPreprocess(getNistSource("RW101A.CBL"))
    )

    @Test
    fun rw102A() = rewriteRun(
        cobolPreprocess(getNistSource("RW102A.CBL"))
    )

    @Test
    fun rw103A() = rewriteRun(
        cobolPreprocess(getNistSource("RW103A.CBL"))
    )

    @Test
    fun rw104A() = rewriteRun(
        cobolPreprocess(getNistSource("RW104A.CBL"))
    )

    @Test
    fun rw301M() = rewriteRun(
        cobolPreprocess(getNistSource("RW301M.CBL"))
    )

    @Test
    fun rw302M() = rewriteRun(
        cobolPreprocess(getNistSource("RW302M.CBL"))
    )

    @Test
    fun sg101A() = rewriteRun(
        cobolPreprocess(getNistSource("SG101A.CBL"))
    )

    @Test
    fun sg102A() = rewriteRun(
        cobolPreprocess(getNistSource("SG102A.CBL"))
    )

    @Test
    fun sg103A() = rewriteRun(
        cobolPreprocess(getNistSource("SG103A.CBL"))
    )

    @Test
    fun sg104A() = rewriteRun(
        cobolPreprocess(getNistSource("SG104A.CBL"))
    )

    @Test
    fun sg105A() = rewriteRun(
        cobolPreprocess(getNistSource("SG105A.CBL"))
    )

    @Test
    fun sg106A() = rewriteRun(
        cobolPreprocess(getNistSource("SG106A.CBL"))
    )

    @Test
    fun sg201A() = rewriteRun(
        cobolPreprocess(getNistSource("SG201A.CBL"))
    )

    @Test
    fun sg202A() = rewriteRun(
        cobolPreprocess(getNistSource("SG202A.CBL"))
    )

    @Test
    fun sg203A() = rewriteRun(
        cobolPreprocess(getNistSource("SG203A.CBL"))
    )

    @Test
    fun sg204A() = rewriteRun(
        cobolPreprocess(getNistSource("SG204A.CBL"))
    )

    @Test
    fun sm102A() = rewriteRun(
        cobolPreprocess(getNistSource("SM102A.CBL"))
    )

    @Test
    fun sm104A() = rewriteRun(
        cobolPreprocess(getNistSource("SM104A.CBL"))
    )

    @Test
    fun sm204A() = rewriteRun(
        cobolPreprocess(getNistSource("SM204A.CBL"))
    )

    @Test
    fun sq101M() = rewriteRun(
        cobolPreprocess(getNistSource("SQ101M.CBL"))
    )

    @Test
    fun sq102A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ102A.CBL"))
    )

    @Test
    fun sq103A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ103A.CBL"))
    )

    @Test
    fun sq104A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ104A.CBL"))
    )

    @Test
    fun sq105A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ105A.CBL"))
    )

    @Test
    fun sq106A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ106A.CBL"))
    )

    @Test
    fun sq107A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ107A.CBL"))
    )

    @Test
    fun sq108A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ108A.CBL"))
    )

    @Test
    fun sq109M() = rewriteRun(
        cobolPreprocess(getNistSource("SQ109M.CBL"))
    )

    @Test
    fun sq110M() = rewriteRun(
        cobolPreprocess(getNistSource("SQ110M.CBL"))
    )

    @Test
    fun sq111A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ111A.CBL"))
    )

    @Test
    fun sq112A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ112A.CBL"))
    )

    @Test
    fun sq113A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ113A.CBL"))
    )

    @Test
    fun sq114A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ114A.CBL"))
    )

    @Test
    fun sq115A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ115A.CBL"))
    )

    @Test
    fun sq116A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ116A.CBL"))
    )

    @Test
    fun sq117A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ117A.CBL"))
    )

    @Test
    fun sq121A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ121A.CBL"))
    )

    @Test
    fun sq122A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ122A.CBL"))
    )

    @Test
    fun sq123A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ123A.CBL"))
    )

    @Test
    fun sq124A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ124A.CBL"))
    )

    @Test
    fun sq125A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ125A.CBL"))
    )

    @Test
    fun sq126A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ126A.CBL"))
    )

    @Test
    fun sq127A() = rewriteRun(
            cobolPreprocess(getNistSource("SQ127A.CBL"))
    )

    @Test
    fun sq128A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ128A.CBL"))
    )

    @Test
    fun sq129A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ129A.CBL"))
    )

    @Test
    fun sq130A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ130A.CBL"))
    )

    @Test
    fun sq131A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ131A.CBL"))
    )

    @Test
    fun sq132A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ132A.CBL"))
    )

    @Test
    fun sq133A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ133A.CBL"))
    )

    @Test
    fun sq134A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ134A.CBL"))
    )

    @Test
    fun sq135A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ135A.CBL"))
    )

    @Test
    fun sq136A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ136A.CBL"))
    )

    @Test
    fun sq137A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ137A.CBL"))
    )

    @Test
    fun sq138A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ138A.CBL"))
    )

    @Test
    fun sq139A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ139A.CBL"))
    )

    @Test
    fun sq140A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ140A.CBL"))
    )

    @Test
    fun sq141A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ141A.CBL"))
    )

    @Test
    fun sq142A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ142A.CBL"))
    )

    @Test
    fun sq143A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ143A.CBL"))
    )

    @Test
    fun sq144A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ144A.CBL"))
    )

    @Test
    fun sq146A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ146A.CBL"))
    )

    @Test
    fun sq147A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ147A.CBL"))
    )

    @Test
    fun sq148A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ148A.CBL"))
    )

    @Test
    fun sq149A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ149A.CBL"))
    )

    @Test
    fun sq150A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ150A.CBL"))
    )

    @Test
    fun sq151A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ151A.CBL"))
    )

    @Test
    fun sq152A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ152A.CBL"))
    )

    @Test
    fun sq153A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ153A.CBL"))
    )

    @Test
    fun sq154A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ154A.CBL"))
    )

    @Test
    fun sq155A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ155A.CBL"))
    )

    @Test
    fun sq156A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ156A.CBL"))
    )

    @Test
    fun sq201M() = rewriteRun(
        cobolPreprocess(getNistSource("SQ201M.CBL"))
    )

    @Test
    fun sq202A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ202A.CBL"))
    )

    @Test
    fun sq203A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ203A.CBL"))
    )

    @Test
    fun sq204A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ204A.CBL"))
    )

    @Test
    fun sq205A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ205A.CBL"))
    )

    @Test
    fun sq206A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ206A.CBL"))
    )

    @Test
    fun sq207M() = rewriteRun(
        cobolPreprocess(getNistSource("SQ207M.CBL"))
    )

    @Test
    fun sq208M() = rewriteRun(
        cobolPreprocess(getNistSource("SQ208M.CBL"))
    )

    @Test
    fun sq209M() = rewriteRun(
        cobolPreprocess(getNistSource("SQ209M.CBL"))
    )

    @Test
    fun sq210M() = rewriteRun(
        cobolPreprocess(getNistSource("SQ210M.CBL"))
    )

    @Test
    fun sq211A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ211A.CBL"))
    )

    @Test
    fun sq212A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ212A.CBL"))
    )

    @Test
    fun sq213A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ213A.CBL"))
    )

    @Test
    fun sq214A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ214A.CBL"))
    )

    @Test
    fun sq215A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ215A.CBL"))
    )

    @Test
    fun sq216A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ216A.CBL"))
    )

    @Test
    fun sq217A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ217A.CBL"))
    )

    @Test
    fun sq218A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ218A.CBL"))
    )

    @Test
    fun sq219A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ219A.CBL"))
    )

    @Test
    fun sq220A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ220A.CBL"))
    )

    @Test
    fun sq221A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ221A.CBL"))
    )

    @Test
    fun sq222A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ222A.CBL"))
    )

    @Test
    fun sq223A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ223A.CBL"))
    )

    @Test
    fun sq224A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ224A.CBL"))
    )

    @Test
    fun sq225A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ225A.CBL"))
    )

    @Test
    fun sq226A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ226A.CBL"))
    )

    @Test
    fun sq227A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ227A.CBL"))
    )

    @Test
    fun sq228A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ228A.CBL"))
    )

    @Test
    fun sq229A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ229A.CBL"))
    )

    @Test
    fun sq230A() = rewriteRun(
        cobolPreprocess(getNistSource("SQ230A.CBL"))
    )

    @Test
    fun sq302M() = rewriteRun(
        cobolPreprocess(getNistSource("SQ302M.CBL"))
    )

    @Test
    fun sq303M() = rewriteRun(
        cobolPreprocess(getNistSource("SQ303M.CBL"))
    )

    @Test
    fun sq401M() = rewriteRun(
        cobolPreprocess(getNistSource("SQ401M.CBL"))
    )

    @Test
    fun st101A() = rewriteRun(
        cobolPreprocess(getNistSource("st101A.CBL"))
    )

    @Test
    fun st102A() = rewriteRun(
        cobolPreprocess(getNistSource("st102A.CBL"))
    )

    @Test
    fun st103A() = rewriteRun(
        cobolPreprocess(getNistSource("st103A.CBL"))
    )

    @Test
    fun st104A() = rewriteRun(
        cobolPreprocess(getNistSource("st104A.CBL"))
    )

    @Test
    fun st105A() = rewriteRun(
        cobolPreprocess(getNistSource("st105A.CBL"))
    )

    @Test
    fun st106A() = rewriteRun(
        cobolPreprocess(getNistSource("st106A.CBL"))
    )

    @Test
    fun st107A() = rewriteRun(
        cobolPreprocess(getNistSource("st107A.CBL"))
    )

    @Test
    fun st108A() = rewriteRun(
        cobolPreprocess(getNistSource("st108A.CBL"))
    )

    @Test
    fun st109A() = rewriteRun(
        cobolPreprocess(getNistSource("st109A.CBL"))
    )

    @Test
    fun st110A() = rewriteRun(
        cobolPreprocess(getNistSource("st110A.CBL"))
    )

    @Test
    fun st111A() = rewriteRun(
        cobolPreprocess(getNistSource("st111A.CBL"))
    )

    @Test
    fun st112M() = rewriteRun(
        cobolPreprocess(getNistSource("st112M.CBL"))
    )

    @Test
    fun st113M() = rewriteRun(
        cobolPreprocess(getNistSource("st113M.CBL"))
    )

    @Test
    fun st114M() = rewriteRun(
        cobolPreprocess(getNistSource("st114M.CBL"))
    )

    @Test
    fun st115A() = rewriteRun(
        cobolPreprocess(getNistSource("st115A.CBL"))
    )

    @Test
    fun st116A() = rewriteRun(
        cobolPreprocess(getNistSource("st116A.CBL"))
    )

    @Test
    fun st117A() = rewriteRun(
        cobolPreprocess(getNistSource("st117A.CBL"))
    )

    @Test
    fun st118A() = rewriteRun(
        cobolPreprocess(getNistSource("st118A.CBL"))
    )

    @Test
    fun st119A() = rewriteRun(
        cobolPreprocess(getNistSource("st119A.CBL"))
    )

    @Test
    fun st120A() = rewriteRun(
        cobolPreprocess(getNistSource("st120A.CBL"))
    )

    @Test
    fun st121A() = rewriteRun(
        cobolPreprocess(getNistSource("st121A.CBL"))
    )

    @Test
    fun st122A() = rewriteRun(
        cobolPreprocess(getNistSource("st122A.CBL"))
    )

    @Test
    fun st123A() = rewriteRun(
        cobolPreprocess(getNistSource("st123A.CBL"))
    )

    @Test
    fun st124A() = rewriteRun(
        cobolPreprocess(getNistSource("st124A.CBL"))
    )

    @Test
    fun st125A() = rewriteRun(
        cobolPreprocess(getNistSource("st125A.CBL"))
    )

    @Test
    fun st126A() = rewriteRun(
        cobolPreprocess(getNistSource("st126A.CBL"))
    )

    @Test
    fun st127A() = rewriteRun(
        cobolPreprocess(getNistSource("st127A.CBL"))
    )

    @Test
    fun st131A() = rewriteRun(
        cobolPreprocess(getNistSource("st131A.CBL"))
    )

    @Test
    fun st132A() = rewriteRun(
        cobolPreprocess(getNistSource("st132A.CBL"))
    )

    @Test
    fun st133A() = rewriteRun(
        cobolPreprocess(getNistSource("st133A.CBL"))
    )

    @Test
    fun st134A() = rewriteRun(
        cobolPreprocess(getNistSource("st134A.CBL"))
    )

    @Test
    fun st135A() = rewriteRun(
        cobolPreprocess(getNistSource("st135A.CBL"))
    )

    @Test
    fun st136A() = rewriteRun(
        cobolPreprocess(getNistSource("st136A.CBL"))
    )

    @Test
    fun st137A() = rewriteRun(
        cobolPreprocess(getNistSource("st137A.CBL"))
    )

    @Test
    fun st139A() = rewriteRun(
        cobolPreprocess(getNistSource("st139A.CBL"))
    )

    @Test
    fun st140A() = rewriteRun(
        cobolPreprocess(getNistSource("st140A.CBL"))
    )

    @Test
    fun st144A() = rewriteRun(
        cobolPreprocess(getNistSource("st144A.CBL"))
    )

    @Test
    fun st146A() = rewriteRun(
        cobolPreprocess(getNistSource("st146A.CBL"))
    )

    @Test
    fun st147A() = rewriteRun(
        cobolPreprocess(getNistSource("st147A.CBL"))
    )

    @Test
    fun st301M() = rewriteRun(
        cobolPreprocess(getNistSource("st301M.CBL"))
    )
}
