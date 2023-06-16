/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.cobol.tree.cobol;

import org.junit.jupiter.api.Test;
import org.openrewrite.cobol.CobolTest;

import static org.openrewrite.cobol.Assertions.cobol;

class CobolParserNistTest extends CobolTest {

    @Test
    void cm101M() {
        rewriteRun(
          cobol(getNistResource("CM101M.CBL"))
        );
    }

    @Test
    void cm102M() {
        rewriteRun(
          cobol(getNistResource("CM102M.CBL"))
        );
    }

    @Test
    void cm103M() {
        rewriteRun(
          cobol(getNistResource("CM103M.CBL"))
        );
    }

    @Test
    void cm104M() {
        rewriteRun(
          cobol(getNistResource("CM104M.CBL"))
        );
    }

    @Test
    void cm105M() {
        rewriteRun(
          cobol(getNistResource("CM105M.CBL"))
        );
    }

    @Test
    void cm201M() {
        rewriteRun(
          cobol(getNistResource("CM201M.CBL"))
        );
    }

    @Test
    void cm202M() {
        rewriteRun(
          cobol(getNistResource("CM202M.CBL"))
        );
    }

    @Test
    void cm303M() {
        rewriteRun(
          cobol(getNistResource("CM303M.CBL"))
        );
    }

    @Test
    void cm401M() {
        rewriteRun(
          cobol(getNistResource("CM401M.CBL"))
        );
    }

    @Test
    void db101A() {
        rewriteRun(
          cobol(getNistResource("DB101A.CBL"))
        );
    }

    @Test
    void db102A() {
        rewriteRun(
          cobol(getNistResource("DB102A.CBL"))
        );
    }

    @Test
    void db103M() {
        rewriteRun(
          cobol(getNistResource("DB103M.CBL"))
        );
    }

    @Test
    void db104A() {
        rewriteRun(
          cobol(getNistResource("DB104A.CBL"))
        );
    }

    @Test
    void db105A() {
        rewriteRun(
          cobol(getNistResource("DB105A.CBL"))
        );
    }

    @Test
    void db201A() {
        rewriteRun(
          cobol(getNistResource("DB201A.CBL"))
        );
    }

    @Test
    void db202A() {
        rewriteRun(
          cobol(getNistResource("DB202A.CBL"))
        );
    }

    @Test
    void db203A() {
        rewriteRun(
          cobol(getNistResource("DB203A.CBL"))
        );
    }

    @Test
    void db204A() {
        rewriteRun(
          cobol(getNistResource("DB204A.CBL"))
        );
    }

    @Test
    void db205A() {
        rewriteRun(
          cobol(getNistResource("DB205A.CBL"))
        );
    }

    @Test
    void db301M() {
        rewriteRun(
          cobol(getNistResource("DB301M.CBL"))
        );
    }

    @Test
    void db302M() {
        rewriteRun(
          cobol(getNistResource("DB302M.CBL"))
        );
    }

    @Test
    void db303M() {
        rewriteRun(
          cobol(getNistResource("DB303M.CBL"))
        );
    }

    @Test
    void db304M() {
        rewriteRun(
          cobol(getNistResource("DB304M.CBL"))
        );
    }

    @Test
    void db305M() {
        rewriteRun(
          cobol(getNistResource("DB305M.CBL"))
        );
    }

    @Test
    void exec85() {
        rewriteRun(
          cobol(getNistResource("EXEC85.CBL"))
        );
    }

    @Test
    void ic101A() {
        rewriteRun(
          cobol(getNistResource("IC101A.CBL"))
        );
    }

    @Test
    void ic102A() {
        rewriteRun(
          cobol(getNistResource("IC102A.CBL"))
        );
    }

    @Test
    void ic103A() {
        rewriteRun(
          cobol(getNistResource("IC103A.CBL"))
        );
    }

    @Test
    void ic104A() {
        rewriteRun(
          cobol(getNistResource("IC104A.CBL"))
        );
    }

    @Test
    void ic105A() {
        rewriteRun(
          cobol(getNistResource("IC105A.CBL"))
        );
    }

    @Test
    void ic106A() {
        rewriteRun(
          cobol(getNistResource("IC106A.CBL"))
        );
    }

    @Test
    void ic107A() {
        rewriteRun(
          cobol(getNistResource("IC107A.CBL"))
        );
    }

    @Test
    void ic108A() {
        rewriteRun(
          cobol(getNistResource("IC108A.CBL"))
        );
    }

    @Test
    void ic109A() {
        rewriteRun(
          cobol(getNistResource("IC109A.CBL"))
        );
    }

    @Test
    void ic110A() {
        rewriteRun(
          cobol(getNistResource("IC110A.CBL"))
        );
    }

    @Test
    void ic111A() {
        rewriteRun(
          cobol(getNistResource("IC111A.CBL"))
        );
    }

    @Test
    void ic112A() {
        rewriteRun(
          cobol(getNistResource("IC112A.CBL"))
        );
    }

    @Test
    void ic113A() {
        rewriteRun(
          cobol(getNistResource("IC113A.CBL"))
        );
    }

    @Test
    void ic114A() {
        rewriteRun(
          cobol(getNistResource("IC114A.CBL"))
        );
    }

    @Test
    void ic115A() {
        rewriteRun(
          cobol(getNistResource("IC115A.CBL"))
        );
    }

    @Test
    void ic116M() {
        rewriteRun(
          cobol(getNistResource("IC116M.CBL"))
        );
    }

    @Test
    void ic117M() {
        rewriteRun(
          cobol(getNistResource("IC117M.CBL"))
        );
    }

    @Test
    void ic118M() {
        rewriteRun(
          cobol(getNistResource("IC118M.CBL"))
        );
    }

    @Test
    void ic201A() {
        rewriteRun(
          cobol(getNistResource("IC201A.CBL"))
        );
    }

    @Test
    void ic202A() {
        rewriteRun(
          cobol(getNistResource("IC202A.CBL"))
        );
    }

    @Test
    void ic203A() {
        rewriteRun(
          cobol(getNistResource("IC203A.CBL"))
        );
    }

    @Test
    void ic204A() {
        rewriteRun(
          cobol(getNistResource("IC204A.CBL"))
        );
    }

    @Test
    void ic205A() {
        rewriteRun(
          cobol(getNistResource("IC205A.CBL"))
        );
    }

    @Test
    void ic206A() {
        rewriteRun(
          cobol(getNistResource("IC206A.CBL"))
        );
    }

    @Test
    void ic207A() {
        rewriteRun(
          cobol(getNistResource("IC207A.CBL"))
        );
    }

    @Test
    void ic208A() {
        rewriteRun(
          cobol(getNistResource("IC208A.CBL"))
        );
    }

    @Test
    void ic209A() {
        rewriteRun(
          cobol(getNistResource("IC209A.CBL"))
        );
    }

    @Test
    void ic210A() {
        rewriteRun(
          cobol(getNistResource("IC210A.CBL"))
        );
    }

    @Test
    void ic211A() {
        rewriteRun(
          cobol(getNistResource("IC211A.CBL"))
        );
    }

    @Test
    void ic212A() {
        rewriteRun(
          cobol(getNistResource("IC212A.CBL"))
        );
    }

    @Test
    void ic213A() {
        rewriteRun(
          cobol(getNistResource("IC213A.CBL"))
        );
    }

    @Test
    void ic214A() {
        rewriteRun(
          cobol(getNistResource("IC214A.CBL"))
        );
    }

    @Test
    void ic215A() {
        rewriteRun(
          cobol(getNistResource("IC215A.CBL"))
        );
    }

    @Test
    void ic216A() {
        rewriteRun(
          cobol(getNistResource("IC216A.CBL"))
        );
    }

    @Test
    void ic217A() {
        rewriteRun(
          cobol(getNistResource("IC217A.CBL"))
        );
    }

    @Test
    void ic222A() {
        rewriteRun(
          cobol(getNistResource("IC222A.CBL"))
        );
    }

    @Test
    void ic223A() {
        rewriteRun(
          cobol(getNistResource("IC223A.CBL"))
        );
    }

    @Test
    void ic224A() {
        rewriteRun(
          cobol(getNistResource("IC224A.CBL"))
        );
    }

    @Test
    void ic225A() {
        rewriteRun(
          cobol(getNistResource("IC225A.CBL"))
        );
    }

    @Test
    void ic226A() {
        rewriteRun(
          cobol(getNistResource("IC226A.CBL"))
        );
    }

    @Test
    void ic227A() {
        rewriteRun(
          cobol(getNistResource("IC227A.CBL"))
        );
    }

    @Test
    void ic228A() {
        rewriteRun(
          cobol(getNistResource("IC228A.CBL"))
        );
    }

    @Test
    void ic233A() {
        rewriteRun(
          cobol(getNistResource("IC233A.CBL"))
        );
    }

    @Test
    void ic234A() {
        rewriteRun(
          cobol(getNistResource("IC234A.CBL"))
        );
    }

    @Test
    void ic235A() {
        rewriteRun(
          cobol(getNistResource("IC235A.CBL"))
        );
    }

    @Test
    void ic237A() {
        rewriteRun(
          cobol(getNistResource("IC237A.CBL"))
        );
    }

    @Test
    void ic401M() {
        rewriteRun(
          cobol(getNistResource("IC401M.CBL"))
        );
    }

    @Test
    void if101A() {
        rewriteRun(
          cobol(getNistResource("IF101A.CBL"))
        );
    }

    @Test
    void if102A() {
        rewriteRun(
          cobol(getNistResource("IF102A.CBL"))
        );
    }

    @Test
    void if103A() {
        rewriteRun(
          cobol(getNistResource("IF103A.CBL"))
        );
    }

    @Test
    void if104A() {
        rewriteRun(
          cobol(getNistResource("IF104A.CBL"))
        );
    }

    @Test
    void if105A() {
        rewriteRun(
          cobol(getNistResource("IF105A.CBL"))
        );
    }

    @Test
    void if106A() {
        rewriteRun(
          cobol(getNistResource("IF106A.CBL"))
        );
    }

    @Test
    void if107A() {
        rewriteRun(
          cobol(getNistResource("IF107A.CBL"))
        );
    }

    @Test
    void if108A() {
        rewriteRun(
          cobol(getNistResource("IF108A.CBL"))
        );
    }

    @Test
    void if109A() {
        rewriteRun(
          cobol(getNistResource("IF109A.CBL"))
        );
    }

    @Test
    void if110A() {
        rewriteRun(
          cobol(getNistResource("IF110A.CBL"))
        );
    }

    @Test
    void if111A() {
        rewriteRun(
          cobol(getNistResource("IF111A.CBL"))
        );
    }

    @Test
    void if112A() {
        rewriteRun(
          cobol(getNistResource("IF112A.CBL"))
        );
    }

    @Test
    void if113A() {
        rewriteRun(
          cobol(getNistResource("IF113A.CBL"))
        );
    }

    @Test
    void if114A() {
        rewriteRun(
          cobol(getNistResource("IF114A.CBL"))
        );
    }

    @Test
    void if115A() {
        rewriteRun(
          cobol(getNistResource("IF115A.CBL"))
        );
    }

    @Test
    void if116A() {
        rewriteRun(
          cobol(getNistResource("IF116A.CBL"))
        );
    }

    @Test
    void if117A() {
        rewriteRun(
          cobol(getNistResource("IF117A.CBL"))
        );
    }

    @Test
    void if118A() {
        rewriteRun(
          cobol(getNistResource("IF118A.CBL"))
        );
    }

    @Test
    void if119A() {
        rewriteRun(
          cobol(getNistResource("IF119A.CBL"))
        );
    }

    @Test
    void if120A() {
        rewriteRun(
          cobol(getNistResource("IF120A.CBL"))
        );
    }

    @Test
    void if121A() {
        rewriteRun(
          cobol(getNistResource("IF121A.CBL"))
        );
    }

    @Test
    void if122A() {
        rewriteRun(
          cobol(getNistResource("IF122A.CBL"))
        );
    }

    @Test
    void if123A() {
        rewriteRun(
          cobol(getNistResource("IF123A.CBL"))
        );
    }

    @Test
    void if124A() {
        rewriteRun(
          cobol(getNistResource("IF124A.CBL"))
        );
    }

    @Test
    void if125A() {
        rewriteRun(
          cobol(getNistResource("IF125A.CBL"))
        );
    }

    @Test
    void if126A() {
        rewriteRun(
          cobol(getNistResource("IF126A.CBL"))
        );
    }

    @Test
    void if127A() {
        rewriteRun(
          cobol(getNistResource("IF127A.CBL"))
        );
    }

    @Test
    void if128A() {
        rewriteRun(
          cobol(getNistResource("IF128A.CBL"))
        );
    }

    @Test
    void if129A() {
        rewriteRun(
          cobol(getNistResource("IF129A.CBL"))
        );
    }

    @Test
    void if130A() {
        rewriteRun(
          cobol(getNistResource("IF130A.CBL"))
        );
    }

    @Test
    void if131A() {
        rewriteRun(
          cobol(getNistResource("IF131A.CBL"))
        );
    }

    @Test
    void if132A() {
        rewriteRun(
          cobol(getNistResource("IF132A.CBL"))
        );
    }

    @Test
    void if133A() {
        rewriteRun(
          cobol(getNistResource("IF133A.CBL"))
        );
    }

    @Test
    void if134A() {
        rewriteRun(
          cobol(getNistResource("IF134A.CBL"))
        );
    }

    @Test
    void if135A() {
        rewriteRun(
          cobol(getNistResource("IF135A.CBL"))
        );
    }

    @Test
    void if136A() {
        rewriteRun(
          cobol(getNistResource("IF136A.CBL"))
        );
    }

    @Test
    void if137A() {
        rewriteRun(
          cobol(getNistResource("IF137A.CBL"))
        );
    }

    @Test
    void if138A() {
        rewriteRun(
          cobol(getNistResource("IF138A.CBL"))
        );
    }

    @Test
    void if139A() {
        rewriteRun(
          cobol(getNistResource("IF139A.CBL"))
        );
    }

    @Test
    void if140A() {
        rewriteRun(
          cobol(getNistResource("IF140A.CBL"))
        );
    }

    @Test
    void if141A() {
        rewriteRun(
          cobol(getNistResource("IF141A.CBL"))
        );
    }

    @Test
    void if142A() {
        rewriteRun(
          cobol(getNistResource("IF142A.CBL"))
        );
    }

    @Test
    void if401M() {
        rewriteRun(
          cobol(getNistResource("IF401M.CBL"))
        );
    }

    @Test
    void if402M() {
        rewriteRun(
          cobol(getNistResource("IF402M.CBL"))
        );
    }

    @Test
    void if403M() {
        rewriteRun(
          cobol(getNistResource("IF403M.CBL"))
        );
    }

    @Test
    void ix101A() {
        rewriteRun(
          cobol(getNistResource("IX101A.CBL"))
        );
    }

    @Test
    void ix102A() {
        rewriteRun(
          cobol(getNistResource("IX102A.CBL"))
        );
    }

    @Test
    void ix103A() {
        rewriteRun(
          cobol(getNistResource("IX103A.CBL"))
        );
    }

    @Test
    void ix104A() {
        rewriteRun(
          cobol(getNistResource("IX104A.CBL"))
        );
    }

    @Test
    void ix105A() {
        rewriteRun(
          cobol(getNistResource("IX105A.CBL"))
        );
    }

    @Test
    void ix106A() {
        rewriteRun(
          cobol(getNistResource("IX106A.CBL"))
        );
    }

    @Test
    void ix107A() {
        rewriteRun(
          cobol(getNistResource("IX107A.CBL"))
        );
    }

    @Test
    void ix108A() {
        rewriteRun(
          cobol(getNistResource("IX108A.CBL"))
        );
    }

    @Test
    void ix109A() {
        rewriteRun(
          cobol(getNistResource("IX109A.CBL"))
        );
    }

    @Test
    void ix110A() {
        rewriteRun(
          cobol(getNistResource("IX110A.CBL"))
        );
    }

    @Test
    void ix111A() {
        rewriteRun(
          cobol(getNistResource("IX111A.CBL"))
        );
    }

    @Test
    void ix112A() {
        rewriteRun(
          cobol(getNistResource("IX112A.CBL"))
        );
    }

    @Test
    void ix113A() {
        rewriteRun(
          cobol(getNistResource("IX113A.CBL"))
        );
    }

    @Test
    void ix114A() {
        rewriteRun(
          cobol(getNistResource("IX114A.CBL"))
        );
    }

    @Test
    void ix115A() {
        rewriteRun(
          cobol(getNistResource("IX115A.CBL"))
        );
    }

    @Test
    void ix116A() {
        rewriteRun(
          cobol(getNistResource("IX116A.CBL"))
        );
    }

    @Test
    void ix117A() {
        rewriteRun(
          cobol(getNistResource("IX117A.CBL"))
        );
    }

    @Test
    void ix118A() {
        rewriteRun(
          cobol(getNistResource("IX118A.CBL"))
        );
    }

    @Test
    void ix119A() {
        rewriteRun(
          cobol(getNistResource("IX119A.CBL"))
        );
    }

    @Test
    void ix120A() {
        rewriteRun(
          cobol(getNistResource("IX120A.CBL"))
        );
    }

    @Test
    void ix121A() {
        rewriteRun(
          cobol(getNistResource("IX121A.CBL"))
        );
    }

    @Test
    void ix201A() {
        rewriteRun(
          cobol(getNistResource("IX201A.CBL"))
        );
    }

    @Test
    void ix202A() {
        rewriteRun(
          cobol(getNistResource("IX202A.CBL"))
        );
    }

    @Test
    void ix203A() {
        rewriteRun(
          cobol(getNistResource("IX203A.CBL"))
        );
    }

    @Test
    void ix204A() {
        rewriteRun(
          cobol(getNistResource("IX204A.CBL"))
        );
    }

    @Test
    void ix205A() {
        rewriteRun(
          cobol(getNistResource("IX205A.CBL"))
        );
    }

    @Test
    void ix206A() {
        rewriteRun(
          cobol(getNistResource("IX206A.CBL"))
        );
    }

    @Test
    void ix207A() {
        rewriteRun(
          cobol(getNistResource("IX207A.CBL"))
        );
    }

    @Test
    void ix208A() {
        rewriteRun(
          cobol(getNistResource("IX208A.CBL"))
        );
    }

    @Test
    void ix209A() {
        rewriteRun(
          cobol(getNistResource("IX209A.CBL"))
        );
    }

    @Test
    void ix210A() {
        rewriteRun(
          cobol(getNistResource("IX210A.CBL"))
        );
    }

    @Test
    void ix211A() {
        rewriteRun(
          cobol(getNistResource("IX211A.CBL"))
        );
    }

    @Test
    void ix212A() {
        rewriteRun(
          cobol(getNistResource("IX212A.CBL"))
        );
    }

    @Test
    void ix213A() {
        rewriteRun(
          cobol(getNistResource("IX213A.CBL"))
        );
    }

    @Test
    void ix214A() {
        rewriteRun(
          cobol(getNistResource("IX214A.CBL"))
        );
    }

    @Test
    void ix215A() {
        rewriteRun(
          cobol(getNistResource("IX215A.CBL"))
        );
    }

    @Test
    void ix216A() {
        rewriteRun(
          cobol(getNistResource("IX216A.CBL"))
        );
    }

    @Test
    void ix217A() {
        rewriteRun(
          cobol(getNistResource("IX217A.CBL"))
        );
    }

    @Test
    void ix218A() {
        rewriteRun(
          cobol(getNistResource("IX218A.CBL"))
        );
    }

    @Test
    void ix301M() {
        rewriteRun(
          cobol(getNistResource("IX301M.CBL"))
        );
    }

    @Test
    void ix302M() {
        rewriteRun(
          cobol(getNistResource("IX302M.CBL"))
        );
    }

    @Test
    void ix401M() {
        rewriteRun(
          cobol(getNistResource("IX401M.CBL"))
        );
    }

    @Test
    void nc101A() {
        rewriteRun(
          cobol(getNistResource("NC101A.CBL"))
        );
    }

    @Test
    void nc102A() {
        rewriteRun(
          cobol(getNistResource("NC102A.CBL"))
        );
    }

    @Test
    void nc103A() {
        rewriteRun(
          cobol(getNistResource("NC103A.CBL"))
        );
    }

    @Test
    void nc104A() {
        rewriteRun(
          cobol(getNistResource("NC104A.CBL"))
        );
    }

    @Test
    void nc105A() {
        rewriteRun(
          cobol(getNistResource("NC105A.CBL"))
        );
    }

    @Test
    void nc106A() {
        rewriteRun(
          cobol(getNistResource("NC106A.CBL"))
        );
    }

    @Test
    void nc107A() {
        rewriteRun(
          cobol(getNistResource("NC107A.CBL"))
        );
    }

    @Test
    void nc108M() {
        rewriteRun(
          cobol(getNistResource("NC108M.CBL"))
        );
    }

    @Test
    void nc109M() {
        rewriteRun(
          cobol(getNistResource("NC109M.CBL"))
        );
    }

    @Test
    void nc110M() {
        rewriteRun(
          cobol(getNistResource("NC110M.CBL"))
        );
    }

    @Test
    void nc111A() {
        rewriteRun(
          cobol(getNistResource("NC111A.CBL"))
        );
    }

    @Test
    void nc112A() {
        rewriteRun(
          cobol(getNistResource("NC112A.CBL"))
        );
    }

    @Test
    void nc113M() {
        rewriteRun(
          cobol(getNistResource("NC113M.CBL"))
        );
    }

    @Test
    void nc114M() {
        rewriteRun(
          cobol(getNistResource("NC114M.CBL"))
        );
    }

    @Test
    void nc115A() {
        rewriteRun(
          cobol(getNistResource("NC115A.CBL"))
        );
    }

    @Test
    void nc116A() {
        rewriteRun(
          cobol(getNistResource("NC116A.CBL"))
        );
    }

    @Test
    void nc117A() {
        rewriteRun(
          cobol(getNistResource("NC117A.CBL"))
        );
    }

    @Test
    void nc118A() {
        rewriteRun(
          cobol(getNistResource("NC118A.CBL"))
        );
    }

    @Test
    void nc119A() {
        rewriteRun(
          cobol(getNistResource("NC119A.CBL"))
        );
    }

    @Test
    void nc120A() {
        rewriteRun(
          cobol(getNistResource("NC120A.CBL"))
        );
    }

    @Test
    void nc121M() {
        rewriteRun(
          cobol(getNistResource("NC121M.CBL"))
        );
    }

    @Test
    void nc122A() {
        rewriteRun(
          cobol(getNistResource("NC122A.CBL"))
        );
    }

    @Test
    void nc123A() {
        rewriteRun(
          cobol(getNistResource("NC123A.CBL"))
        );
    }

    @Test
    void nc124A() {
        rewriteRun(
          cobol(getNistResource("NC124A.CBL"))
        );
    }

    @Test
    void nc125A() {
        rewriteRun(
          cobol(getNistResource("NC125A.CBL"))
        );
    }

    @Test
    void nc126A() {
        rewriteRun(
          cobol(getNistResource("NC126A.CBL"))
        );
    }

    @Test
    void nc127A() {
        rewriteRun(
          cobol(getNistResource("NC127A.CBL"))
        );
    }

    @Test
    void nc131A() {
        rewriteRun(
          cobol(getNistResource("NC131A.CBL"))
        );
    }

    @Test
    void nc132A() {
        rewriteRun(
          cobol(getNistResource("NC132A.CBL"))
        );
    }

    @Test
    void nc133A() {
        rewriteRun(
          cobol(getNistResource("NC133A.CBL"))
        );
    }

    @Test
    void nc134A() {
        rewriteRun(
          cobol(getNistResource("NC134A.CBL"))
        );
    }

    @Test
    void nc135A() {
        rewriteRun(
          cobol(getNistResource("NC135A.CBL"))
        );
    }

    @Test
    void nc136A() {
        rewriteRun(
          cobol(getNistResource("NC136A.CBL"))
        );
    }

    @Test
    void nc137A() {
        rewriteRun(
          cobol(getNistResource("NC137A.CBL"))
        );
    }

    @Test
    void nc138A() {
        rewriteRun(
          cobol(getNistResource("NC138A.CBL"))
        );
    }

    @Test
    void nc139A() {
        rewriteRun(
          cobol(getNistResource("NC139A.CBL"))
        );
    }

    @Test
    void nc140A() {
        rewriteRun(
          cobol(getNistResource("NC140A.CBL"))
        );
    }

    @Test
    void nc141A() {
        rewriteRun(
          cobol(getNistResource("NC141A.CBL"))
        );
    }

    @Test
    void nc171A() {
        rewriteRun(
          cobol(getNistResource("NC171A.CBL"))
        );
    }

    @Test
    void nc172A() {
        rewriteRun(
          cobol(getNistResource("NC172A.CBL"))
        );
    }

    @Test
    void nc173A() {
        rewriteRun(
          cobol(getNistResource("NC173A.CBL"))
        );
    }

    @Test
    void nc174A() {
        rewriteRun(
          cobol(getNistResource("NC174A.CBL"))
        );
    }

    @Test
    void nc175A() {
        rewriteRun(
          cobol(getNistResource("NC175A.CBL"))
        );
    }

    @Test
    void nc176A() {
        rewriteRun(
          cobol(getNistResource("NC176A.CBL"))
        );
    }

    @Test
    void nc177A() {
        rewriteRun(
          cobol(getNistResource("NC177A.CBL"))
        );
    }

    @Test
    void nc201A() {
        rewriteRun(
          cobol(getNistResource("NC201A.CBL"))
        );
    }

    @Test
    void nc202A() {
        rewriteRun(
          cobol(getNistResource("NC202A.CBL"))
        );
    }

    @Test
    void nc203A() {
        rewriteRun(
          cobol(getNistResource("NC203A.CBL"))
        );
    }

    @Test
    void nc204M() {
        rewriteRun(
          cobol(getNistResource("NC204M.CBL"))
        );
    }

    @Test
    void nc205A() {
        rewriteRun(
          cobol(getNistResource("NC205A.CBL"))
        );
    }

    @Test
    void nc206A() {
        rewriteRun(
          cobol(getNistResource("NC206A.CBL"))
        );
    }

    @Test
    void nc207A() {
        rewriteRun(
          cobol(getNistResource("NC207A.CBL"))
        );
    }

    @Test
    void nc208A() {
        rewriteRun(
          cobol(getNistResource("NC208A.CBL"))
        );
    }

    @Test
    void nc209A() {
        rewriteRun(
          cobol(getNistResource("NC209A.CBL"))
        );
    }

    @Test
    void nc210A() {
        rewriteRun(
          cobol(getNistResource("NC210A.CBL"))
        );
    }

    @Test
    void nc211A() {
        rewriteRun(
          cobol(getNistResource("NC211A.CBL"))
        );
    }

    @Test
    void nc214M() {
        rewriteRun(
          cobol(getNistResource("NC214M.CBL"))
        );
    }

    @Test
    void nc215A() {
        rewriteRun(
          cobol(getNistResource("NC215A.CBL"))
        );
    }

    @Test
    void nc216A() {
        rewriteRun(
          cobol(getNistResource("NC216A.CBL"))
        );
    }

    @Test
    void nc217A() {
        rewriteRun(
          cobol(getNistResource("NC217A.CBL"))
        );
    }

    @Test
    void nc218A() {
        rewriteRun(
          cobol(getNistResource("NC218A.CBL"))
        );
    }

    @Test
    void nc219A() {
        rewriteRun(
          cobol(getNistResource("NC219A.CBL"))
        );
    }

    @Test
    void nc220M() {
        rewriteRun(
          cobol(getNistResource("NC220M.CBL"))
        );
    }

    @Test
    void nc221A() {
        rewriteRun(
          cobol(getNistResource("NC221A.CBL"))
        );
    }

    @Test
    void nc222A() {
        rewriteRun(
          cobol(getNistResource("NC222A.CBL"))
        );
    }

    @Test
    void nc223A() {
        rewriteRun(
          cobol(getNistResource("NC223A.CBL"))
        );
    }

    @Test
    void nc224A() {
        rewriteRun(
          cobol(getNistResource("NC224A.CBL"))
        );
    }

    @Test
    void nc225A() {
        rewriteRun(
          cobol(getNistResource("NC225A.CBL"))
        );
    }

    @Test
    void nc231A() {
        rewriteRun(
          cobol(getNistResource("NC231A.CBL"))
        );
    }

    @Test
    void nc232A() {
        rewriteRun(
          cobol(getNistResource("NC232A.CBL"))
        );
    }

    @Test
    void nc233A() {
        rewriteRun(
          cobol(getNistResource("NC233A.CBL"))
        );
    }

    @Test
    void nc234A() {
        rewriteRun(
          cobol(getNistResource("NC234A.CBL"))
        );
    }

    @Test
    void nc235A() {
        rewriteRun(
          cobol(getNistResource("NC235A.CBL"))
        );
    }

    @Test
    void nc236A() {
        rewriteRun(
          cobol(getNistResource("NC236A.CBL"))
        );
    }

    @Test
    void nc237A() {
        rewriteRun(
          cobol(getNistResource("NC237A.CBL"))
        );
    }

    @Test
    void nc238A() {
        rewriteRun(
          cobol(getNistResource("NC238A.CBL"))
        );
    }

    @Test
    void nc239A() {
        rewriteRun(
          cobol(getNistResource("NC239A.CBL"))
        );
    }

    @Test
    void nc240A() {
        rewriteRun(
          cobol(getNistResource("NC240A.CBL"))
        );
    }

    @Test
    void nc241A() {
        rewriteRun(
          cobol(getNistResource("NC241A.CBL"))
        );
    }

    @Test
    void nc242A() {
        rewriteRun(
          cobol(getNistResource("NC242A.CBL"))
        );
    }

    @Test
    void nc243A() {
        rewriteRun(
          cobol(getNistResource("NC243A.CBL"))
        );
    }

    @Test
    void nc244A() {
        rewriteRun(
          cobol(getNistResource("NC244A.CBL"))
        );
    }

    @Test
    void nc245A() {
        rewriteRun(
          cobol(getNistResource("NC245A.CBL"))
        );
    }

    @Test
    void nc246A() {
        rewriteRun(
          cobol(getNistResource("NC246A.CBL"))
        );
    }

    @Test
    void nc247A() {
        rewriteRun(
          cobol(getNistResource("NC247A.CBL"))
        );
    }

    @Test
    void nc248A() {
        rewriteRun(
          cobol(getNistResource("NC248A.CBL"))
        );
    }

    @Test
    void nc250A() {
        rewriteRun(
          cobol(getNistResource("NC250A.CBL"))
        );
    }

    @Test
    void nc251A() {
        rewriteRun(
          cobol(getNistResource("NC251A.CBL"))
        );
    }

    @Test
    void nc252A() {
        rewriteRun(
          cobol(getNistResource("NC252A.CBL"))
        );
    }

    @Test
    void nc253A() {
        rewriteRun(
          cobol(getNistResource("NC253A.CBL"))
        );
    }

    @Test
    void nc254A() {
        rewriteRun(
          cobol(getNistResource("NC254A.CBL"))
        );
    }

    @Test
    void nc302M() {
        rewriteRun(
          cobol(getNistResource("NC302M.CBL"))
        );
    }

    @Test
    void nc303M() {
        rewriteRun(
          cobol(getNistResource("NC303M.CBL"))
        );
    }

    @Test
    void nc401M() {
        rewriteRun(
          cobol(getNistResource("NC401M.CBL"))
        );
    }

    @Test
    void obic1A() {
        rewriteRun(
          cobol(getNistResource("OBIC1A.CBL"))
        );
    }

    @Test
    void obic2A() {
        rewriteRun(
          cobol(getNistResource("OBIC2A.CBL"))
        );
    }

    @Test
    void obic3A() {
        rewriteRun(
          cobol(getNistResource("OBIC3A.CBL"))
        );
    }

    @Test
    void obnc1M() {
        rewriteRun(
          cobol(getNistResource("OBNC1M.CBL"))
        );
    }

    @Test
    void obnc2M() {
        rewriteRun(
          cobol(getNistResource("OBNC2M.CBL"))
        );
    }

    @Test
    void obsq1A() {
        rewriteRun(
          cobol(getNistResource("OBSQ1A.CBL"))
        );
    }

    @Test
    void obsq3A() {
        rewriteRun(
          cobol(getNistResource("OBSQ3A.CBL"))
        );
    }

    @Test
    void obsq4A() {
        rewriteRun(
          cobol(getNistResource("OBSQ4A.CBL"))
        );
    }

    @Test
    void obsq5A() {
        rewriteRun(
          cobol(getNistResource("OBSQ5A.CBL"))
        );
    }

    @Test
    void rl101A() {
        rewriteRun(
          cobol(getNistResource("RL101A.CBL"))
        );
    }

    @Test
    void rl102A() {
        rewriteRun(
          cobol(getNistResource("RL102A.CBL"))
        );
    }

    @Test
    void rl103A() {
        rewriteRun(
          cobol(getNistResource("RL103A.CBL"))
        );
    }

    @Test
    void rl104A() {
        rewriteRun(
          cobol(getNistResource("RL104A.CBL"))
        );
    }

    @Test
    void rl105A() {
        rewriteRun(
          cobol(getNistResource("RL105A.CBL"))
        );
    }

    @Test
    void rl106A() {
        rewriteRun(
          cobol(getNistResource("RL106A.CBL"))
        );
    }

    @Test
    void rl107A() {
        rewriteRun(
          cobol(getNistResource("RL107A.CBL"))
        );
    }

    @Test
    void rl108A() {
        rewriteRun(
          cobol(getNistResource("RL108A.CBL"))
        );
    }

    @Test
    void rl109A() {
        rewriteRun(
          cobol(getNistResource("RL109A.CBL"))
        );
    }

    @Test
    void rl110A() {
        rewriteRun(
          cobol(getNistResource("RL110A.CBL"))
        );
    }

    @Test
    void rl111A() {
        rewriteRun(
          cobol(getNistResource("RL111A.CBL"))
        );
    }

    @Test
    void rl112A() {
        rewriteRun(
          cobol(getNistResource("RL112A.CBL"))
        );
    }

    @Test
    void rl113A() {
        rewriteRun(
          cobol(getNistResource("RL113A.CBL"))
        );
    }

    @Test
    void rl114A() {
        rewriteRun(
          cobol(getNistResource("RL114A.CBL"))
        );
    }

    @Test
    void rl115A() {
        rewriteRun(
          cobol(getNistResource("RL115A.CBL"))
        );
    }

    @Test
    void rl116A() {
        rewriteRun(
          cobol(getNistResource("RL116A.CBL"))
        );
    }

    @Test
    void rl117A() {
        rewriteRun(
          cobol(getNistResource("RL117A.CBL"))
        );
    }

    @Test
    void rl118A() {
        rewriteRun(
          cobol(getNistResource("RL118A.CBL"))
        );
    }

    @Test
    void rl119A() {
        rewriteRun(
          cobol(getNistResource("RL119A.CBL"))
        );
    }

    @Test
    void rl201A() {
        rewriteRun(
          cobol(getNistResource("RL201A.CBL"))
        );
    }

    @Test
    void rl202A() {
        rewriteRun(
          cobol(getNistResource("RL202A.CBL"))
        );
    }

    @Test
    void rl203A() {
        rewriteRun(
          cobol(getNistResource("RL203A.CBL"))
        );
    }

    @Test
    void rl204A() {
        rewriteRun(
          cobol(getNistResource("RL204A.CBL"))
        );
    }

    @Test
    void rl205A() {
        rewriteRun(
          cobol(getNistResource("RL205A.CBL"))
        );
    }

    @Test
    void rl206A() {
        rewriteRun(
          cobol(getNistResource("RL206A.CBL"))
        );
    }

    @Test
    void rl207A() {
        rewriteRun(
          cobol(getNistResource("RL207A.CBL"))
        );
    }

    @Test
    void rl208A() {
        rewriteRun(
          cobol(getNistResource("RL208A.CBL"))
        );
    }

    @Test
    void rl209A() {
        rewriteRun(
          cobol(getNistResource("RL209A.CBL"))
        );
    }

    @Test
    void rl210A() {
        rewriteRun(
          cobol(getNistResource("RL210A.CBL"))
        );
    }

    @Test
    void rl211A() {
        rewriteRun(
          cobol(getNistResource("RL211A.CBL"))
        );
    }

    @Test
    void rl212A() {
        rewriteRun(
          cobol(getNistResource("RL212A.CBL"))
        );
    }

    @Test
    void rl213A() {
        rewriteRun(
          cobol(getNistResource("RL213A.CBL"))
        );
    }

    @Test
    void rl301M() {
        rewriteRun(
          cobol(getNistResource("RL301M.CBL"))
        );
    }

    @Test
    void rl302M() {
        rewriteRun(
          cobol(getNistResource("RL302M.CBL"))
        );
    }

    @Test
    void rl401M() {
        rewriteRun(
          cobol(getNistResource("RL401M.CBL"))
        );
    }

    @Test
    void rw101A() {
        rewriteRun(
          cobol(getNistResource("RW101A.CBL"))
        );
    }

    @Test
    void rw102A() {
        rewriteRun(
          cobol(getNistResource("RW102A.CBL"))
        );
    }

    @Test
    void rw103A() {
        rewriteRun(
          cobol(getNistResource("RW103A.CBL"))
        );
    }

    @Test
    void rw104A() {
        rewriteRun(
          cobol(getNistResource("RW104A.CBL"))
        );
    }

    @Test
    void rw301M() {
        rewriteRun(
          cobol(getNistResource("RW301M.CBL"))
        );
    }

    @Test
    void rw302M() {
        rewriteRun(
          cobol(getNistResource("RW302M.CBL"))
        );
    }

    @Test
    void sg101A() {
        rewriteRun(
          cobol(getNistResource("SG101A.CBL"))
        );
    }

    @Test
    void sg102A() {
        rewriteRun(
          cobol(getNistResource("SG102A.CBL"))
        );
    }

    @Test
    void sg103A() {
        rewriteRun(
          cobol(getNistResource("SG103A.CBL"))
        );
    }

    @Test
    void sg104A() {
        rewriteRun(
          cobol(getNistResource("SG104A.CBL"))
        );
    }

    @Test
    void sg105A() {
        rewriteRun(
          cobol(getNistResource("SG105A.CBL"))
        );
    }

    @Test
    void sg106A() {
        rewriteRun(
          cobol(getNistResource("SG106A.CBL"))
        );
    }

    @Test
    void sg201A() {
        rewriteRun(
          cobol(getNistResource("SG201A.CBL"))
        );
    }

    @Test
    void sg202A() {
        rewriteRun(
          cobol(getNistResource("SG202A.CBL"))
        );
    }

    @Test
    void sg203A() {
        rewriteRun(
          cobol(getNistResource("SG203A.CBL"))
        );
    }

    @Test
    void sg204A() {
        rewriteRun(
          cobol(getNistResource("SG204A.CBL"))
        );
    }

    @Test
    void sm102A() {
        rewriteRun(
          cobol(getNistResource("SM102A.CBL"))
        );
    }

    @Test
    void sm104A() {
        rewriteRun(
          cobol(getNistResource("SM104A.CBL"))
        );
    }

    @Test
    void sm204A() {
        rewriteRun(
          cobol(getNistResource("SM204A.CBL"))
        );
    }

    @Test
    void sq101M() {
        rewriteRun(
          cobol(getNistResource("SQ101M.CBL"))
        );
    }

    @Test
    void sq102A() {
        rewriteRun(
          cobol(getNistResource("SQ102A.CBL"))
        );
    }

    @Test
    void sq103A() {
        rewriteRun(
          cobol(getNistResource("SQ103A.CBL"))
        );
    }

    @Test
    void sq104A() {
        rewriteRun(
          cobol(getNistResource("SQ104A.CBL"))
        );
    }

    @Test
    void sq105A() {
        rewriteRun(
          cobol(getNistResource("SQ105A.CBL"))
        );
    }

    @Test
    void sq106A() {
        rewriteRun(
          cobol(getNistResource("SQ106A.CBL"))
        );
    }

    @Test
    void sq107A() {
        rewriteRun(
          cobol(getNistResource("SQ107A.CBL"))
        );
    }

    @Test
    void sq108A() {
        rewriteRun(
          cobol(getNistResource("SQ108A.CBL"))
        );
    }

    @Test
    void sq109M() {
        rewriteRun(
          cobol(getNistResource("SQ109M.CBL"))
        );
    }

    @Test
    void sq110M() {
        rewriteRun(
          cobol(getNistResource("SQ110M.CBL"))
        );
    }

    @Test
    void sq111A() {
        rewriteRun(
          cobol(getNistResource("SQ111A.CBL"))
        );
    }

    @Test
    void sq112A() {
        rewriteRun(
          cobol(getNistResource("SQ112A.CBL"))
        );
    }

    @Test
    void sq113A() {
        rewriteRun(
          cobol(getNistResource("SQ113A.CBL"))
        );
    }

    @Test
    void sq114A() {
        rewriteRun(
          cobol(getNistResource("SQ114A.CBL"))
        );
    }

    @Test
    void sq115A() {
        rewriteRun(
          cobol(getNistResource("SQ115A.CBL"))
        );
    }

    @Test
    void sq116A() {
        rewriteRun(
          cobol(getNistResource("SQ116A.CBL"))
        );
    }

    @Test
    void sq117A() {
        rewriteRun(
          cobol(getNistResource("SQ117A.CBL"))
        );
    }

    @Test
    void sq121A() {
        rewriteRun(
          cobol(getNistResource("SQ121A.CBL"))
        );
    }

    @Test
    void sq122A() {
        rewriteRun(
          cobol(getNistResource("SQ122A.CBL"))
        );
    }

    @Test
    void sq123A() {
        rewriteRun(
          cobol(getNistResource("SQ123A.CBL"))
        );
    }

    @Test
    void sq124A() {
        rewriteRun(
          cobol(getNistResource("SQ124A.CBL"))
        );
    }

    @Test
    void sq125A() {
        rewriteRun(
          cobol(getNistResource("SQ125A.CBL"))
        );
    }

    @Test
    void sq126A() {
        rewriteRun(
          cobol(getNistResource("SQ126A.CBL"))
        );
    }

    @Test
    void sq127A() {
        rewriteRun(
          cobol(getNistResource("SQ127A.CBL"))
        );
    }

    @Test
    void sq128A() {
        rewriteRun(
          cobol(getNistResource("SQ128A.CBL"))
        );
    }

    @Test
    void sq129A() {
        rewriteRun(
          cobol(getNistResource("SQ129A.CBL"))
        );
    }

    @Test
    void sq130A() {
        rewriteRun(
          cobol(getNistResource("SQ130A.CBL"))
        );
    }

    @Test
    void sq131A() {
        rewriteRun(
          cobol(getNistResource("SQ131A.CBL"))
        );
    }

    @Test
    void sq132A() {
        rewriteRun(
          cobol(getNistResource("SQ132A.CBL"))
        );
    }

    @Test
    void sq133A() {
        rewriteRun(
          cobol(getNistResource("SQ133A.CBL"))
        );
    }

    @Test
    void sq134A() {
        rewriteRun(
          cobol(getNistResource("SQ134A.CBL"))
        );
    }

    @Test
    void sq135A() {
        rewriteRun(
          cobol(getNistResource("SQ135A.CBL"))
        );
    }

    @Test
    void sq136A() {
        rewriteRun(
          cobol(getNistResource("SQ136A.CBL"))
        );
    }

    @Test
    void sq137A() {
        rewriteRun(
          cobol(getNistResource("SQ137A.CBL"))
        );
    }

    @Test
    void sq138A() {
        rewriteRun(
          cobol(getNistResource("SQ138A.CBL"))
        );
    }

    @Test
    void sq139A() {
        rewriteRun(
          cobol(getNistResource("SQ139A.CBL"))
        );
    }

    @Test
    void sq140A() {
        rewriteRun(
          cobol(getNistResource("SQ140A.CBL"))
        );
    }

    @Test
    void sq141A() {
        rewriteRun(
          cobol(getNistResource("SQ141A.CBL"))
        );
    }

    @Test
    void sq142A() {
        rewriteRun(
          cobol(getNistResource("SQ142A.CBL"))
        );
    }

    @Test
    void sq143A() {
        rewriteRun(
          cobol(getNistResource("SQ143A.CBL"))
        );
    }

    @Test
    void sq144A() {
        rewriteRun(
          cobol(getNistResource("SQ144A.CBL"))
        );
    }

    @Test
    void sq146A() {
        rewriteRun(
          cobol(getNistResource("SQ146A.CBL"))
        );
    }

    @Test
    void sq147A() {
        rewriteRun(
          cobol(getNistResource("SQ147A.CBL"))
        );
    }

    @Test
    void sq148A() {
        rewriteRun(
          cobol(getNistResource("SQ148A.CBL"))
        );
    }

    @Test
    void sq149A() {
        rewriteRun(
          cobol(getNistResource("SQ149A.CBL"))
        );
    }

    @Test
    void sq150A() {
        rewriteRun(
          cobol(getNistResource("SQ150A.CBL"))
        );
    }

    @Test
    void sq151A() {
        rewriteRun(
          cobol(getNistResource("SQ151A.CBL"))
        );
    }

    @Test
    void sq152A() {
        rewriteRun(
          cobol(getNistResource("SQ152A.CBL"))
        );
    }

    @Test
    void sq153A() {
        rewriteRun(
          cobol(getNistResource("SQ153A.CBL"))
        );
    }

    @Test
    void sq154A() {
        rewriteRun(
          cobol(getNistResource("SQ154A.CBL"))
        );
    }

    @Test
    void sq155A() {
        rewriteRun(
          cobol(getNistResource("SQ155A.CBL"))
        );
    }

    @Test
    void sq156A() {
        rewriteRun(
          cobol(getNistResource("SQ156A.CBL"))
        );
    }

    @Test
    void sq201M() {
        rewriteRun(
          cobol(getNistResource("SQ201M.CBL"))
        );
    }

    @Test
    void sq202A() {
        rewriteRun(
          cobol(getNistResource("SQ202A.CBL"))
        );
    }

    @Test
    void sq203A() {
        rewriteRun(
          cobol(getNistResource("SQ203A.CBL"))
        );
    }

    @Test
    void sq204A() {
        rewriteRun(
          cobol(getNistResource("SQ204A.CBL"))
        );
    }

    @Test
    void sq205A() {
        rewriteRun(
          cobol(getNistResource("SQ205A.CBL"))
        );
    }

    @Test
    void sq206A() {
        rewriteRun(
          cobol(getNistResource("SQ206A.CBL"))
        );
    }

    @Test
    void sq207M() {
        rewriteRun(
          cobol(getNistResource("SQ207M.CBL"))
        );
    }

    @Test
    void sq208M() {
        rewriteRun(
          cobol(getNistResource("SQ208M.CBL"))
        );
    }

    @Test
    void sq209M() {
        rewriteRun(
          cobol(getNistResource("SQ209M.CBL"))
        );
    }

    @Test
    void sq210M() {
        rewriteRun(
          cobol(getNistResource("SQ210M.CBL"))
        );
    }

    @Test
    void sq211A() {
        rewriteRun(
          cobol(getNistResource("SQ211A.CBL"))
        );
    }

    @Test
    void sq212A() {
        rewriteRun(
          cobol(getNistResource("SQ212A.CBL"))
        );
    }

    @Test
    void sq213A() {
        rewriteRun(
          cobol(getNistResource("SQ213A.CBL"))
        );
    }

    @Test
    void sq214A() {
        rewriteRun(
          cobol(getNistResource("SQ214A.CBL"))
        );
    }

    @Test
    void sq215A() {
        rewriteRun(
          cobol(getNistResource("SQ215A.CBL"))
        );
    }

    @Test
    void sq216A() {
        rewriteRun(
          cobol(getNistResource("SQ216A.CBL"))
        );
    }

    @Test
    void sq217A() {
        rewriteRun(
          cobol(getNistResource("SQ217A.CBL"))
        );
    }

    @Test
    void sq218A() {
        rewriteRun(
          cobol(getNistResource("SQ218A.CBL"))
        );
    }

    @Test
    void sq219A() {
        rewriteRun(
          cobol(getNistResource("SQ219A.CBL"))
        );
    }

    @Test
    void sq220A() {
        rewriteRun(
          cobol(getNistResource("SQ220A.CBL"))
        );
    }

    @Test
    void sq221A() {
        rewriteRun(
          cobol(getNistResource("SQ221A.CBL"))
        );
    }

    @Test
    void sq222A() {
        rewriteRun(
          cobol(getNistResource("SQ222A.CBL"))
        );
    }

    @Test
    void sq223A() {
        rewriteRun(
          cobol(getNistResource("SQ223A.CBL"))
        );
    }

    @Test
    void sq224A() {
        rewriteRun(
          cobol(getNistResource("SQ224A.CBL"))
        );
    }

    @Test
    void sq225A() {
        rewriteRun(
          cobol(getNistResource("SQ225A.CBL"))
        );
    }

    @Test
    void sq226A() {
        rewriteRun(
          cobol(getNistResource("SQ226A.CBL"))
        );
    }

    @Test
    void sq227A() {
        rewriteRun(
          cobol(getNistResource("SQ227A.CBL"))
        );
    }

    @Test
    void sq228A() {
        rewriteRun(
          cobol(getNistResource("SQ228A.CBL"))
        );
    }

    @Test
    void sq229A() {
        rewriteRun(
          cobol(getNistResource("SQ229A.CBL"))
        );
    }

    @Test
    void sq230A() {
        rewriteRun(
          cobol(getNistResource("SQ230A.CBL"))
        );
    }

    @Test
    void sq302M() {
        rewriteRun(
          cobol(getNistResource("SQ302M.CBL"))
        );
    }

    @Test
    void sq303M() {
        rewriteRun(
          cobol(getNistResource("SQ303M.CBL"))
        );
    }

    @Test
    void sq401M() {
        rewriteRun(
          cobol(getNistResource("SQ401M.CBL"))
        );
    }

    @Test
    void st101A() {
        rewriteRun(
          cobol(getNistResource("ST101A.CBL"))
        );
    }

    @Test
    void st102A() {
        rewriteRun(
          cobol(getNistResource("ST102A.CBL"))
        );
    }

    @Test
    void st103A() {
        rewriteRun(
          cobol(getNistResource("ST103A.CBL"))
        );
    }

    @Test
    void st104A() {
        rewriteRun(
          cobol(getNistResource("ST104A.CBL"))
        );
    }

    @Test
    void st105A() {
        rewriteRun(
          cobol(getNistResource("ST105A.CBL"))
        );
    }

    @Test
    void st106A() {
        rewriteRun(
          cobol(getNistResource("ST106A.CBL"))
        );
    }

    @Test
    void st107A() {
        rewriteRun(
          cobol(getNistResource("ST107A.CBL"))
        );
    }

    @Test
    void st108A() {
        rewriteRun(
          cobol(getNistResource("ST108A.CBL"))
        );
    }

    @Test
    void st109A() {
        rewriteRun(
          cobol(getNistResource("ST109A.CBL"))
        );
    }

    @Test
    void st110A() {
        rewriteRun(
          cobol(getNistResource("ST110A.CBL"))
        );
    }

    @Test
    void st111A() {
        rewriteRun(
          cobol(getNistResource("ST111A.CBL"))
        );
    }

    @Test
    void st112M() {
        rewriteRun(
          cobol(getNistResource("ST112M.CBL"))
        );
    }

    @Test
    void st113M() {
        rewriteRun(
          cobol(getNistResource("ST113M.CBL"))
        );
    }

    @Test
    void st114M() {
        rewriteRun(
          cobol(getNistResource("ST114M.CBL"))
        );
    }

    @Test
    void st115A() {
        rewriteRun(
          cobol(getNistResource("ST115A.CBL"))
        );
    }

    @Test
    void st116A() {
        rewriteRun(
          cobol(getNistResource("ST116A.CBL"))
        );
    }

    @Test
    void st117A() {
        rewriteRun(
          cobol(getNistResource("ST117A.CBL"))
        );
    }

    @Test
    void st118A() {
        rewriteRun(
          cobol(getNistResource("ST118A.CBL"))
        );
    }

    @Test
    void st119A() {
        rewriteRun(
          cobol(getNistResource("ST119A.CBL"))
        );
    }

    @Test
    void st120A() {
        rewriteRun(
          cobol(getNistResource("ST120A.CBL"))
        );
    }

    @Test
    void st121A() {
        rewriteRun(
          cobol(getNistResource("ST121A.CBL"))
        );
    }

    @Test
    void st122A() {
        rewriteRun(
          cobol(getNistResource("ST122A.CBL"))
        );
    }

    @Test
    void st123A() {
        rewriteRun(
          cobol(getNistResource("ST123A.CBL"))
        );
    }

    @Test
    void st124A() {
        rewriteRun(
          cobol(getNistResource("ST124A.CBL"))
        );
    }

    @Test
    void st125A() {
        rewriteRun(
          cobol(getNistResource("ST125A.CBL"))
        );
    }

    @Test
    void st126A() {
        rewriteRun(
          cobol(getNistResource("ST126A.CBL"))
        );
    }

    @Test
    void st127A() {
        rewriteRun(
          cobol(getNistResource("ST127A.CBL"))
        );
    }

    @Test
    void st131A() {
        rewriteRun(
          cobol(getNistResource("ST131A.CBL"))
        );
    }

    @Test
    void st132A() {
        rewriteRun(
          cobol(getNistResource("ST132A.CBL"))
        );
    }

    @Test
    void st133A() {
        rewriteRun(
          cobol(getNistResource("ST133A.CBL"))
        );
    }

    @Test
    void st134A() {
        rewriteRun(
          cobol(getNistResource("ST134A.CBL"))
        );
    }

    @Test
    void st135A() {
        rewriteRun(
          cobol(getNistResource("ST135A.CBL"))
        );
    }

    @Test
    void st136A() {
        rewriteRun(
          cobol(getNistResource("ST136A.CBL"))
        );
    }

    @Test
    void st137A() {
        rewriteRun(
          cobol(getNistResource("ST137A.CBL"))
        );
    }

    @Test
    void st139A() {
        rewriteRun(
          cobol(getNistResource("ST139A.CBL"))
        );
    }

    @Test
    void st140A() {
        rewriteRun(
          cobol(getNistResource("ST140A.CBL"))
        );
    }

    @Test
    void st144A() {
        rewriteRun(
          cobol(getNistResource("ST144A.CBL"))
        );
    }

    @Test
    void st146A() {
        rewriteRun(
          cobol(getNistResource("ST146A.CBL"))
        );
    }

    @Test
    void st147A() {
        rewriteRun(
          cobol(getNistResource("ST147A.CBL"))
        );
    }

    @Test
    void st301M() {
        rewriteRun(
          cobol(getNistResource("ST301M.CBL"))
        );
    }
}
