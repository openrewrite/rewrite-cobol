/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.cobol.tree.cobol;

import org.junit.jupiter.api.Test;
import org.openrewrite.cobol.CobolTest;

import static org.openrewrite.cobol.Assertions.cobol;
import static org.openrewrite.cobol.Assertions.preprocessor;

public class CobolPreprocessorNistTest extends CobolTest {

    @Test
    void cm101M() {
        rewriteRun(
          preprocessor(getNistResource("CM101M.CBL"))
        );
    }

    @Test
    void cm102M() {
        rewriteRun(
          preprocessor(getNistResource("CM102M.CBL"))
        );
    }

    @Test
    void cm103M() {
        rewriteRun(
          preprocessor(getNistResource("CM103M.CBL"))
        );
    }

    @Test
    void cm104M() {
        rewriteRun(
          preprocessor(getNistResource("CM104M.CBL"))
        );
    }

    @Test
    void cm105M() {
        rewriteRun(
          preprocessor(getNistResource("CM105M.CBL"))
        );
    }

    @Test
    void cm201M() {
        rewriteRun(
          preprocessor(getNistResource("CM201M.CBL"))
        );
    }

    @Test
    void cm202M() {
        rewriteRun(
          preprocessor(getNistResource("CM202M.CBL"))
        );
    }

    @Test
    void cm303M() {
        rewriteRun(
          preprocessor(getNistResource("CM303M.CBL"))
        );
    }

    @Test
    void cm401M() {
        rewriteRun(
          preprocessor(getNistResource("CM401M.CBL"))
        );
    }

    @Test
    void db101A() {
        rewriteRun(
          preprocessor(getNistResource("DB101A.CBL"))
        );
    }

    @Test
    void db102A() {
        rewriteRun(
          preprocessor(getNistResource("DB102A.CBL"))
        );
    }

    @Test
    void db103M() {
        rewriteRun(
          preprocessor(getNistResource("DB103M.CBL"))
        );
    }

    @Test
    void db104A() {
        rewriteRun(
          preprocessor(getNistResource("DB104A.CBL"))
        );
    }

    @Test
    void db105A() {
        rewriteRun(
          preprocessor(getNistResource("DB105A.CBL"))
        );
    }

    @Test
    void db201A() {
        rewriteRun(
          preprocessor(getNistResource("DB201A.CBL"))
        );
    }

    @Test
    void db202A() {
        rewriteRun(
          preprocessor(getNistResource("DB202A.CBL"))
        );
    }

    @Test
    void db203A() {
        rewriteRun(
          preprocessor(getNistResource("DB203A.CBL"))
        );
    }

    @Test
    void db204A() {
        rewriteRun(
          preprocessor(getNistResource("DB204A.CBL"))
        );
    }

    @Test
    void db205A() {
        rewriteRun(
          preprocessor(getNistResource("DB205A.CBL"))
        );
    }

    @Test
    void db301M() {
        rewriteRun(
          preprocessor(getNistResource("DB301M.CBL"))
        );
    }

    @Test
    void db302M() {
        rewriteRun(
          preprocessor(getNistResource("DB302M.CBL"))
        );
    }

    @Test
    void db303M() {
        rewriteRun(
          preprocessor(getNistResource("DB303M.CBL"))
        );
    }

    @Test
    void db304M() {
        rewriteRun(
          preprocessor(getNistResource("DB304M.CBL"))
        );
    }

    @Test
    void db305M() {
        rewriteRun(
          preprocessor(getNistResource("DB305M.CBL"))
        );
    }

    @Test
    void exec85() {
        rewriteRun(
          preprocessor(getNistResource("EXEC85.CBL"))
        );
    }

    @Test
    void ic101A() {
        rewriteRun(
          preprocessor(getNistResource("IC101A.CBL"))
        );
    }

    @Test
    void ic102A() {
        rewriteRun(
          preprocessor(getNistResource("IC102A.CBL"))
        );
    }

    @Test
    void ic103A() {
        rewriteRun(
          preprocessor(getNistResource("IC103A.CBL"))
        );
    }

    @Test
    void ic104A() {
        rewriteRun(
          preprocessor(getNistResource("IC104A.CBL"))
        );
    }

    @Test
    void ic105A() {
        rewriteRun(
          preprocessor(getNistResource("IC105A.CBL"))
        );
    }

    @Test
    void ic106A() {
        rewriteRun(
          preprocessor(getNistResource("IC106A.CBL"))
        );
    }

    @Test
    void ic107A() {
        rewriteRun(
          preprocessor(getNistResource("IC107A.CBL"))
        );
    }

    @Test
    void ic108A() {
        rewriteRun(
          preprocessor(getNistResource("IC108A.CBL"))
        );
    }

    @Test
    void ic109A() {
        rewriteRun(
          preprocessor(getNistResource("IC109A.CBL"))
        );
    }

    @Test
    void ic110A() {
        rewriteRun(
          preprocessor(getNistResource("IC110A.CBL"))
        );
    }

    @Test
    void ic111A() {
        rewriteRun(
          preprocessor(getNistResource("IC111A.CBL"))
        );
    }

    @Test
    void ic112A() {
        rewriteRun(
          preprocessor(getNistResource("IC112A.CBL"))
        );
    }

    @Test
    void ic113A() {
        rewriteRun(
          preprocessor(getNistResource("IC113A.CBL"))
        );
    }

    @Test
    void ic114A() {
        rewriteRun(
          preprocessor(getNistResource("IC114A.CBL"))
        );
    }

    @Test
    void ic115A() {
        rewriteRun(
          preprocessor(getNistResource("IC115A.CBL"))
        );
    }

    @Test
    void ic116M() {
        rewriteRun(
          preprocessor(getNistResource("IC116M.CBL"))
        );
    }

    @Test
    void ic117M() {
        rewriteRun(
          preprocessor(getNistResource("IC117M.CBL"))
        );
    }

    @Test
    void ic118M() {
        rewriteRun(
          preprocessor(getNistResource("IC118M.CBL"))
        );
    }

    @Test
    void ic201A() {
        rewriteRun(
          preprocessor(getNistResource("IC201A.CBL"))
        );
    }

    @Test
    void ic202A() {
        rewriteRun(
          preprocessor(getNistResource("IC202A.CBL"))
        );
    }

    @Test
    void ic203A() {
        rewriteRun(
          preprocessor(getNistResource("IC203A.CBL"))
        );
    }

    @Test
    void ic204A() {
        rewriteRun(
          preprocessor(getNistResource("IC204A.CBL"))
        );
    }

    @Test
    void ic205A() {
        rewriteRun(
          preprocessor(getNistResource("IC205A.CBL"))
        );
    }

    @Test
    void ic206A() {
        rewriteRun(
          preprocessor(getNistResource("IC206A.CBL"))
        );
    }

    @Test
    void ic207A() {
        rewriteRun(
          preprocessor(getNistResource("IC207A.CBL"))
        );
    }

    @Test
    void ic208A() {
        rewriteRun(
          preprocessor(getNistResource("IC208A.CBL"))
        );
    }

    @Test
    void ic209A() {
        rewriteRun(
          preprocessor(getNistResource("IC209A.CBL"))
        );
    }

    @Test
    void ic210A() {
        rewriteRun(
          preprocessor(getNistResource("IC210A.CBL"))
        );
    }

    @Test
    void ic211A() {
        rewriteRun(
          preprocessor(getNistResource("IC211A.CBL"))
        );
    }

    @Test
    void ic212A() {
        rewriteRun(
          preprocessor(getNistResource("IC212A.CBL"))
        );
    }

    @Test
    void ic213A() {
        rewriteRun(
          preprocessor(getNistResource("IC213A.CBL"))
        );
    }

    @Test
    void ic214A() {
        rewriteRun(
          preprocessor(getNistResource("IC214A.CBL"))
        );
    }

    @Test
    void ic215A() {
        rewriteRun(
          preprocessor(getNistResource("IC215A.CBL"))
        );
    }

    @Test
    void ic216A() {
        rewriteRun(
          preprocessor(getNistResource("IC216A.CBL"))
        );
    }

    @Test
    void ic217A() {
        rewriteRun(
          preprocessor(getNistResource("IC217A.CBL"))
        );
    }

    @Test
    void ic222A() {
        rewriteRun(
          preprocessor(getNistResource("IC222A.CBL"))
        );
    }

    @Test
    void ic223A() {
        rewriteRun(
          preprocessor(getNistResource("IC223A.CBL"))
        );
    }

    @Test
    void ic224A() {
        rewriteRun(
          preprocessor(getNistResource("IC224A.CBL"))
        );
    }

    @Test
    void ic225A() {
        rewriteRun(
          preprocessor(getNistResource("IC225A.CBL"))
        );
    }

    @Test
    void ic226A() {
        rewriteRun(
          preprocessor(getNistResource("IC226A.CBL"))
        );
    }

    @Test
    void ic227A() {
        rewriteRun(
          preprocessor(getNistResource("IC227A.CBL"))
        );
    }

    @Test
    void ic228A() {
        rewriteRun(
          preprocessor(getNistResource("IC228A.CBL"))
        );
    }

    @Test
    void ic233A() {
        rewriteRun(
          preprocessor(getNistResource("IC233A.CBL"))
        );
    }

    @Test
    void ic234A() {
        rewriteRun(
          preprocessor(getNistResource("IC234A.CBL"))
        );
    }

    @Test
    void ic235A() {
        rewriteRun(
          preprocessor(getNistResource("IC235A.CBL"))
        );
    }

    @Test
    void ic237A() {
        rewriteRun(
          preprocessor(getNistResource("IC237A.CBL"))
        );
    }

    @Test
    void ic401M() {
        rewriteRun(
          preprocessor(getNistResource("IC401M.CBL"))
        );
    }

    @Test
    void if101A() {
        rewriteRun(
          preprocessor(getNistResource("IF101A.CBL"))
        );
    }

    @Test
    void if102A() {
        rewriteRun(
          preprocessor(getNistResource("IF102A.CBL"))
        );
    }

    @Test
    void if103A() {
        rewriteRun(
          preprocessor(getNistResource("IF103A.CBL"))
        );
    }

    @Test
    void if104A() {
        rewriteRun(
          preprocessor(getNistResource("IF104A.CBL"))
        );
    }

    @Test
    void if105A() {
        rewriteRun(
          preprocessor(getNistResource("IF105A.CBL"))
        );
    }

    @Test
    void if106A() {
        rewriteRun(
          preprocessor(getNistResource("IF106A.CBL"))
        );
    }

    @Test
    void if107A() {
        rewriteRun(
          preprocessor(getNistResource("IF107A.CBL"))
        );
    }

    @Test
    void if108A() {
        rewriteRun(
          preprocessor(getNistResource("IF108A.CBL"))
        );
    }

    @Test
    void if109A() {
        rewriteRun(
          preprocessor(getNistResource("IF109A.CBL"))
        );
    }

    @Test
    void if110A() {
        rewriteRun(
          preprocessor(getNistResource("IF110A.CBL"))
        );
    }

    @Test
    void if111A() {
        rewriteRun(
          preprocessor(getNistResource("IF111A.CBL"))
        );
    }

    @Test
    void if112A() {
        rewriteRun(
          preprocessor(getNistResource("IF112A.CBL"))
        );
    }

    @Test
    void if113A() {
        rewriteRun(
          preprocessor(getNistResource("IF113A.CBL"))
        );
    }

    @Test
    void if114A() {
        rewriteRun(
          preprocessor(getNistResource("IF114A.CBL"))
        );
    }

    @Test
    void if115A() {
        rewriteRun(
          preprocessor(getNistResource("IF115A.CBL"))
        );
    }

    @Test
    void if116A() {
        rewriteRun(
          preprocessor(getNistResource("IF116A.CBL"))
        );
    }

    @Test
    void if117A() {
        rewriteRun(
          preprocessor(getNistResource("IF117A.CBL"))
        );
    }

    @Test
    void if118A() {
        rewriteRun(
          preprocessor(getNistResource("IF118A.CBL"))
        );
    }

    @Test
    void if119A() {
        rewriteRun(
          preprocessor(getNistResource("IF119A.CBL"))
        );
    }

    @Test
    void if120A() {
        rewriteRun(
          preprocessor(getNistResource("IF120A.CBL"))
        );
    }

    @Test
    void if121A() {
        rewriteRun(
          preprocessor(getNistResource("IF121A.CBL"))
        );
    }

    @Test
    void if122A() {
        rewriteRun(
          preprocessor(getNistResource("IF122A.CBL"))
        );
    }

    @Test
    void if123A() {
        rewriteRun(
          preprocessor(getNistResource("IF123A.CBL"))
        );
    }

    @Test
    void if124A() {
        rewriteRun(
          preprocessor(getNistResource("IF124A.CBL"))
        );
    }

    @Test
    void if125A() {
        rewriteRun(
          preprocessor(getNistResource("IF125A.CBL"))
        );
    }

    @Test
    void if126A() {
        rewriteRun(
          preprocessor(getNistResource("IF126A.CBL"))
        );
    }

    @Test
    void if127A() {
        rewriteRun(
          preprocessor(getNistResource("IF127A.CBL"))
        );
    }

    @Test
    void if128A() {
        rewriteRun(
          preprocessor(getNistResource("IF128A.CBL"))
        );
    }

    @Test
    void if129A() {
        rewriteRun(
          preprocessor(getNistResource("IF129A.CBL"))
        );
    }

    @Test
    void if130A() {
        rewriteRun(
          preprocessor(getNistResource("IF130A.CBL"))
        );
    }

    @Test
    void if131A() {
        rewriteRun(
          preprocessor(getNistResource("IF131A.CBL"))
        );
    }

    @Test
    void if132A() {
        rewriteRun(
          preprocessor(getNistResource("IF132A.CBL"))
        );
    }

    @Test
    void if133A() {
        rewriteRun(
          preprocessor(getNistResource("IF133A.CBL"))
        );
    }

    @Test
    void if134A() {
        rewriteRun(
          preprocessor(getNistResource("IF134A.CBL"))
        );
    }

    @Test
    void if135A() {
        rewriteRun(
          preprocessor(getNistResource("IF135A.CBL"))
        );
    }

    @Test
    void if136A() {
        rewriteRun(
          preprocessor(getNistResource("IF136A.CBL"))
        );
    }

    @Test
    void if137A() {
        rewriteRun(
          preprocessor(getNistResource("IF137A.CBL"))
        );
    }

    @Test
    void if138A() {
        rewriteRun(
          preprocessor(getNistResource("IF138A.CBL"))
        );
    }

    @Test
    void if139A() {
        rewriteRun(
          preprocessor(getNistResource("IF139A.CBL"))
        );
    }

    @Test
    void if140A() {
        rewriteRun(
          preprocessor(getNistResource("IF140A.CBL"))
        );
    }

    @Test
    void if141A() {
        rewriteRun(
          preprocessor(getNistResource("IF141A.CBL"))
        );
    }

    @Test
    void if142A() {
        rewriteRun(
          preprocessor(getNistResource("IF142A.CBL"))
        );
    }

    @Test
    void if401M() {
        rewriteRun(
          preprocessor(getNistResource("IF401M.CBL"))
        );
    }

    @Test
    void if402M() {
        rewriteRun(
          preprocessor(getNistResource("IF402M.CBL"))
        );
    }

    @Test
    void if403M() {
        rewriteRun(
          preprocessor(getNistResource("IF403M.CBL"))
        );
    }

    @Test
    void ix101A() {
        rewriteRun(
          preprocessor(getNistResource("IX101A.CBL"))
        );
    }

    @Test
    void ix102A() {
        rewriteRun(
          preprocessor(getNistResource("IX102A.CBL"))
        );
    }

    @Test
    void ix103A() {
        rewriteRun(
          preprocessor(getNistResource("IX103A.CBL"))
        );
    }

    @Test
    void ix104A() {
        rewriteRun(
          preprocessor(getNistResource("IX104A.CBL"))
        );
    }

    @Test
    void ix105A() {
        rewriteRun(
          preprocessor(getNistResource("IX105A.CBL"))
        );
    }

    @Test
    void ix106A() {
        rewriteRun(
          preprocessor(getNistResource("IX106A.CBL"))
        );
    }

    @Test
    void ix107A() {
        rewriteRun(
          preprocessor(getNistResource("IX107A.CBL"))
        );
    }

    @Test
    void ix108A() {
        rewriteRun(
          preprocessor(getNistResource("IX108A.CBL"))
        );
    }

    @Test
    void ix109A() {
        rewriteRun(
          preprocessor(getNistResource("IX109A.CBL"))
        );
    }

    @Test
    void ix110A() {
        rewriteRun(
          preprocessor(getNistResource("IX110A.CBL"))
        );
    }

    @Test
    void ix111A() {
        rewriteRun(
          preprocessor(getNistResource("IX111A.CBL"))
        );
    }

    @Test
    void ix112A() {
        rewriteRun(
          preprocessor(getNistResource("IX112A.CBL"))
        );
    }

    @Test
    void ix113A() {
        rewriteRun(
          preprocessor(getNistResource("IX113A.CBL"))
        );
    }

    @Test
    void ix114A() {
        rewriteRun(
          preprocessor(getNistResource("IX114A.CBL"))
        );
    }

    @Test
    void ix115A() {
        rewriteRun(
          preprocessor(getNistResource("IX115A.CBL"))
        );
    }

    @Test
    void ix116A() {
        rewriteRun(
          preprocessor(getNistResource("IX116A.CBL"))
        );
    }

    @Test
    void ix117A() {
        rewriteRun(
          preprocessor(getNistResource("IX117A.CBL"))
        );
    }

    @Test
    void ix118A() {
        rewriteRun(
          preprocessor(getNistResource("IX118A.CBL"))
        );
    }

    @Test
    void ix119A() {
        rewriteRun(
          preprocessor(getNistResource("IX119A.CBL"))
        );
    }

    @Test
    void ix120A() {
        rewriteRun(
          preprocessor(getNistResource("IX120A.CBL"))
        );
    }

    @Test
    void ix121A() {
        rewriteRun(
          preprocessor(getNistResource("IX121A.CBL"))
        );
    }

    @Test
    void ix201A() {
        rewriteRun(
          preprocessor(getNistResource("IX201A.CBL"))
        );
    }

    @Test
    void ix202A() {
        rewriteRun(
          preprocessor(getNistResource("IX202A.CBL"))
        );
    }

    @Test
    void ix203A() {
        rewriteRun(
          preprocessor(getNistResource("IX203A.CBL"))
        );
    }

    @Test
    void ix204A() {
        rewriteRun(
          preprocessor(getNistResource("IX204A.CBL"))
        );
    }

    @Test
    void ix205A() {
        rewriteRun(
          preprocessor(getNistResource("IX205A.CBL"))
        );
    }

    @Test
    void ix206A() {
        rewriteRun(
          preprocessor(getNistResource("IX206A.CBL"))
        );
    }

    @Test
    void ix207A() {
        rewriteRun(
          preprocessor(getNistResource("IX207A.CBL"))
        );
    }

    @Test
    void ix208A() {
        rewriteRun(
          preprocessor(getNistResource("IX208A.CBL"))
        );
    }

    @Test
    void ix209A() {
        rewriteRun(
          preprocessor(getNistResource("IX209A.CBL"))
        );
    }

    @Test
    void ix210A() {
        rewriteRun(
          preprocessor(getNistResource("IX210A.CBL"))
        );
    }

    @Test
    void ix211A() {
        rewriteRun(
          preprocessor(getNistResource("IX211A.CBL"))
        );
    }

    @Test
    void ix212A() {
        rewriteRun(
          preprocessor(getNistResource("IX212A.CBL"))
        );
    }

    @Test
    void ix213A() {
        rewriteRun(
          preprocessor(getNistResource("IX213A.CBL"))
        );
    }

    @Test
    void ix214A() {
        rewriteRun(
          preprocessor(getNistResource("IX214A.CBL"))
        );
    }

    @Test
    void ix215A() {
        rewriteRun(
          preprocessor(getNistResource("IX215A.CBL"))
        );
    }

    @Test
    void ix216A() {
        rewriteRun(
          preprocessor(getNistResource("IX216A.CBL"))
        );
    }

    @Test
    void ix217A() {
        rewriteRun(
          preprocessor(getNistResource("IX217A.CBL"))
        );
    }

    @Test
    void ix218A() {
        rewriteRun(
          preprocessor(getNistResource("IX218A.CBL"))
        );
    }

    @Test
    void ix301M() {
        rewriteRun(
          preprocessor(getNistResource("IX301M.CBL"))
        );
    }

    @Test
    void ix302M() {
        rewriteRun(
          preprocessor(getNistResource("IX302M.CBL"))
        );
    }

    @Test
    void ix401M() {
        rewriteRun(
          preprocessor(getNistResource("IX401M.CBL"))
        );
    }

    @Test
    void nc101A() {
        rewriteRun(
          preprocessor(getNistResource("NC101A.CBL"))
        );
    }

    @Test
    void nc102A() {
        rewriteRun(
          preprocessor(getNistResource("NC102A.CBL"))
        );
    }

    @Test
    void nc103A() {
        rewriteRun(
          preprocessor(getNistResource("NC103A.CBL"))
        );
    }

    @Test
    void nc104A() {
        rewriteRun(
          preprocessor(getNistResource("NC104A.CBL"))
        );
    }

    @Test
    void nc105A() {
        rewriteRun(
          preprocessor(getNistResource("NC105A.CBL"))
        );
    }

    @Test
    void nc106A() {
        rewriteRun(
          preprocessor(getNistResource("NC106A.CBL"))
        );
    }

    @Test
    void nc107A() {
        rewriteRun(
          preprocessor(getNistResource("NC107A.CBL"))
        );
    }

    @Test
    void nc108M() {
        rewriteRun(
          preprocessor(getNistResource("NC108M.CBL"))
        );
    }

    @Test
    void nc109M() {
        rewriteRun(
          preprocessor(getNistResource("NC109M.CBL"))
        );
    }

    @Test
    void nc110M() {
        rewriteRun(
          preprocessor(getNistResource("NC110M.CBL"))
        );
    }

    @Test
    void nc111A() {
        rewriteRun(
          preprocessor(getNistResource("NC111A.CBL"))
        );
    }

    @Test
    void nc112A() {
        rewriteRun(
          preprocessor(getNistResource("NC112A.CBL"))
        );
    }

    @Test
    void nc113M() {
        rewriteRun(
          preprocessor(getNistResource("NC113M.CBL"))
        );
    }

    @Test
    void nc114M() {
        rewriteRun(
          preprocessor(getNistResource("NC114M.CBL"))
        );
    }

    @Test
    void nc115A() {
        rewriteRun(
          preprocessor(getNistResource("NC115A.CBL"))
        );
    }

    @Test
    void nc116A() {
        rewriteRun(
          preprocessor(getNistResource("NC116A.CBL"))
        );
    }

    @Test
    void nc117A() {
        rewriteRun(
          preprocessor(getNistResource("NC117A.CBL"))
        );
    }

    @Test
    void nc118A() {
        rewriteRun(
          preprocessor(getNistResource("NC118A.CBL"))
        );
    }

    @Test
    void nc119A() {
        rewriteRun(
          preprocessor(getNistResource("NC119A.CBL"))
        );
    }

    @Test
    void nc120A() {
        rewriteRun(
          preprocessor(getNistResource("NC120A.CBL"))
        );
    }

    @Test
    void nc121M() {
        rewriteRun(
          preprocessor(getNistResource("NC121M.CBL"))
        );
    }

    @Test
    void nc122A() {
        rewriteRun(
          preprocessor(getNistResource("NC122A.CBL"))
        );
    }

    @Test
    void nc123A() {
        rewriteRun(
          preprocessor(getNistResource("NC123A.CBL"))
        );
    }

    @Test
    void nc124A() {
        rewriteRun(
          preprocessor(getNistResource("NC124A.CBL"))
        );
    }

    @Test
    void nc125A() {
        rewriteRun(
          preprocessor(getNistResource("NC125A.CBL"))
        );
    }

    @Test
    void nc126A() {
        rewriteRun(
          preprocessor(getNistResource("NC126A.CBL"))
        );
    }

    @Test
    void nc127A() {
        rewriteRun(
          preprocessor(getNistResource("NC127A.CBL"))
        );
    }

    @Test
    void nc131A() {
        rewriteRun(
          preprocessor(getNistResource("NC131A.CBL"))
        );
    }

    @Test
    void nc132A() {
        rewriteRun(
          preprocessor(getNistResource("NC132A.CBL"))
        );
    }

    @Test
    void nc133A() {
        rewriteRun(
          preprocessor(getNistResource("NC133A.CBL"))
        );
    }

    @Test
    void nc134A() {
        rewriteRun(
          preprocessor(getNistResource("NC134A.CBL"))
        );
    }

    @Test
    void nc135A() {
        rewriteRun(
          preprocessor(getNistResource("NC135A.CBL"))
        );
    }

    @Test
    void nc136A() {
        rewriteRun(
          preprocessor(getNistResource("NC136A.CBL"))
        );
    }

    @Test
    void nc137A() {
        rewriteRun(
          preprocessor(getNistResource("NC137A.CBL"))
        );
    }

    @Test
    void nc138A() {
        rewriteRun(
          preprocessor(getNistResource("NC138A.CBL"))
        );
    }

    @Test
    void nc139A() {
        rewriteRun(
          preprocessor(getNistResource("NC139A.CBL"))
        );
    }

    @Test
    void nc140A() {
        rewriteRun(
          preprocessor(getNistResource("NC140A.CBL"))
        );
    }

    @Test
    void nc141A() {
        rewriteRun(
          preprocessor(getNistResource("NC141A.CBL"))
        );
    }

    @Test
    void nc171A() {
        rewriteRun(
          preprocessor(getNistResource("NC171A.CBL"))
        );
    }

    @Test
    void nc172A() {
        rewriteRun(
          preprocessor(getNistResource("NC172A.CBL"))
        );
    }

    @Test
    void nc173A() {
        rewriteRun(
          preprocessor(getNistResource("NC173A.CBL"))
        );
    }

    @Test
    void nc174A() {
        rewriteRun(
          preprocessor(getNistResource("NC174A.CBL"))
        );
    }

    @Test
    void nc175A() {
        rewriteRun(
          preprocessor(getNistResource("NC175A.CBL"))
        );
    }

    @Test
    void nc176A() {
        rewriteRun(
          preprocessor(getNistResource("NC176A.CBL"))
        );
    }

    @Test
    void nc177A() {
        rewriteRun(
          preprocessor(getNistResource("NC177A.CBL"))
        );
    }

    @Test
    void nc201A() {
        rewriteRun(
          preprocessor(getNistResource("NC201A.CBL"))
        );
    }

    @Test
    void nc202A() {
        rewriteRun(
          preprocessor(getNistResource("NC202A.CBL"))
        );
    }

    @Test
    void nc203A() {
        rewriteRun(
          preprocessor(getNistResource("NC203A.CBL"))
        );
    }

    @Test
    void nc204M() {
        rewriteRun(
          preprocessor(getNistResource("NC204M.CBL"))
        );
    }

    @Test
    void nc205A() {
        rewriteRun(
          preprocessor(getNistResource("NC205A.CBL"))
        );
    }

    @Test
    void nc206A() {
        rewriteRun(
          preprocessor(getNistResource("NC206A.CBL"))
        );
    }

    @Test
    void nc207A() {
        rewriteRun(
          preprocessor(getNistResource("NC207A.CBL"))
        );
    }

    @Test
    void nc208A() {
        rewriteRun(
          preprocessor(getNistResource("NC208A.CBL"))
        );
    }

    @Test
    void nc209A() {
        rewriteRun(
          preprocessor(getNistResource("NC209A.CBL"))
        );
    }

    @Test
    void nc210A() {
        rewriteRun(
          preprocessor(getNistResource("NC210A.CBL"))
        );
    }

    @Test
    void nc211A() {
        rewriteRun(
          preprocessor(getNistResource("NC211A.CBL"))
        );
    }

    @Test
    void nc214M() {
        rewriteRun(
          preprocessor(getNistResource("NC214M.CBL"))
        );
    }

    @Test
    void nc215A() {
        rewriteRun(
          preprocessor(getNistResource("NC215A.CBL"))
        );
    }

    @Test
    void nc216A() {
        rewriteRun(
          preprocessor(getNistResource("NC216A.CBL"))
        );
    }

    @Test
    void nc217A() {
        rewriteRun(
          preprocessor(getNistResource("NC217A.CBL"))
        );
    }

    @Test
    void nc218A() {
        rewriteRun(
          preprocessor(getNistResource("NC218A.CBL"))
        );
    }

    @Test
    void nc219A() {
        rewriteRun(
          preprocessor(getNistResource("NC219A.CBL"))
        );
    }

    @Test
    void nc220M() {
        rewriteRun(
          preprocessor(getNistResource("NC220M.CBL"))
        );
    }

    @Test
    void nc221A() {
        rewriteRun(
          preprocessor(getNistResource("NC221A.CBL"))
        );
    }

    @Test
    void nc222A() {
        rewriteRun(
          preprocessor(getNistResource("NC222A.CBL"))
        );
    }

    @Test
    void nc223A() {
        rewriteRun(
          preprocessor(getNistResource("NC223A.CBL"))
        );
    }

    @Test
    void nc224A() {
        rewriteRun(
          preprocessor(getNistResource("NC224A.CBL"))
        );
    }

    @Test
    void nc225A() {
        rewriteRun(
          preprocessor(getNistResource("NC225A.CBL"))
        );
    }

    @Test
    void nc231A() {
        rewriteRun(
          preprocessor(getNistResource("NC231A.CBL"))
        );
    }

    @Test
    void nc232A() {
        rewriteRun(
          preprocessor(getNistResource("NC232A.CBL"))
        );
    }

    @Test
    void nc233A() {
        rewriteRun(
          preprocessor(getNistResource("NC233A.CBL"))
        );
    }

    @Test
    void nc234A() {
        rewriteRun(
          preprocessor(getNistResource("NC234A.CBL"))
        );
    }

    @Test
    void nc235A() {
        rewriteRun(
          preprocessor(getNistResource("NC235A.CBL"))
        );
    }

    @Test
    void nc236A() {
        rewriteRun(
          preprocessor(getNistResource("NC236A.CBL"))
        );
    }

    @Test
    void nc237A() {
        rewriteRun(
          preprocessor(getNistResource("NC237A.CBL"))
        );
    }

    @Test
    void nc238A() {
        rewriteRun(
          preprocessor(getNistResource("NC238A.CBL"))
        );
    }

    @Test
    void nc239A() {
        rewriteRun(
          preprocessor(getNistResource("NC239A.CBL"))
        );
    }

    @Test
    void nc240A() {
        rewriteRun(
          preprocessor(getNistResource("NC240A.CBL"))
        );
    }

    @Test
    void nc241A() {
        rewriteRun(
          preprocessor(getNistResource("NC241A.CBL"))
        );
    }

    @Test
    void nc242A() {
        rewriteRun(
          preprocessor(getNistResource("NC242A.CBL"))
        );
    }

    @Test
    void nc243A() {
        rewriteRun(
          preprocessor(getNistResource("NC243A.CBL"))
        );
    }

    @Test
    void nc244A() {
        rewriteRun(
          preprocessor(getNistResource("NC244A.CBL"))
        );
    }

    @Test
    void nc245A() {
        rewriteRun(
          preprocessor(getNistResource("NC245A.CBL"))
        );
    }

    @Test
    void nc246A() {
        rewriteRun(
          preprocessor(getNistResource("NC246A.CBL"))
        );
    }

    @Test
    void nc247A() {
        rewriteRun(
          preprocessor(getNistResource("NC247A.CBL"))
        );
    }

    @Test
    void nc248A() {
        rewriteRun(
          preprocessor(getNistResource("NC248A.CBL"))
        );
    }

    @Test
    void nc250A() {
        rewriteRun(
          preprocessor(getNistResource("NC250A.CBL"))
        );
    }

    @Test
    void nc251A() {
        rewriteRun(
          preprocessor(getNistResource("NC251A.CBL"))
        );
    }

    @Test
    void nc252A() {
        rewriteRun(
          preprocessor(getNistResource("NC252A.CBL"))
        );
    }

    @Test
    void nc253A() {
        rewriteRun(
          preprocessor(getNistResource("NC253A.CBL"))
        );
    }

    @Test
    void nc254A() {
        rewriteRun(
          preprocessor(getNistResource("NC254A.CBL"))
        );
    }

    @Test
    void nc302M() {
        rewriteRun(
          preprocessor(getNistResource("NC302M.CBL"))
        );
    }

    @Test
    void nc303M() {
        rewriteRun(
          preprocessor(getNistResource("NC303M.CBL"))
        );
    }

    @Test
    void nc401M() {
        rewriteRun(
          preprocessor(getNistResource("NC401M.CBL"))
        );
    }

    @Test
    void obic1A() {
        rewriteRun(
          preprocessor(getNistResource("OBIC1A.CBL"))
        );
    }

    @Test
    void obic2A() {
        rewriteRun(
          preprocessor(getNistResource("OBIC2A.CBL"))
        );
    }

    @Test
    void obic3A() {
        rewriteRun(
          preprocessor(getNistResource("OBIC3A.CBL"))
        );
    }

    @Test
    void obnc1M() {
        rewriteRun(
          preprocessor(getNistResource("OBNC1M.CBL"))
        );
    }

    @Test
    void obnc2M() {
        rewriteRun(
          preprocessor(getNistResource("OBNC2M.CBL"))
        );
    }

    @Test
    void obsq1A() {
        rewriteRun(
          preprocessor(getNistResource("OBSQ1A.CBL"))
        );
    }

    @Test
    void obsq3A() {
        rewriteRun(
          preprocessor(getNistResource("OBSQ3A.CBL"))
        );
    }

    @Test
    void obsq4A() {
        rewriteRun(
          preprocessor(getNistResource("OBSQ4A.CBL"))
        );
    }

    @Test
    void obsq5A() {
        rewriteRun(
          preprocessor(getNistResource("OBSQ5A.CBL"))
        );
    }

    @Test
    void rl101A() {
        rewriteRun(
          preprocessor(getNistResource("RL101A.CBL"))
        );
    }

    @Test
    void rl102A() {
        rewriteRun(
          preprocessor(getNistResource("RL102A.CBL"))
        );
    }

    @Test
    void rl103A() {
        rewriteRun(
          preprocessor(getNistResource("RL103A.CBL"))
        );
    }

    @Test
    void rl104A() {
        rewriteRun(
          preprocessor(getNistResource("RL104A.CBL"))
        );
    }

    @Test
    void rl105A() {
        rewriteRun(
          preprocessor(getNistResource("RL105A.CBL"))
        );
    }

    @Test
    void rl106A() {
        rewriteRun(
          preprocessor(getNistResource("RL106A.CBL"))
        );
    }

    @Test
    void rl107A() {
        rewriteRun(
          preprocessor(getNistResource("RL107A.CBL"))
        );
    }

    @Test
    void rl108A() {
        rewriteRun(
          preprocessor(getNistResource("RL108A.CBL"))
        );
    }

    @Test
    void rl109A() {
        rewriteRun(
          preprocessor(getNistResource("RL109A.CBL"))
        );
    }

    @Test
    void rl110A() {
        rewriteRun(
          preprocessor(getNistResource("RL110A.CBL"))
        );
    }

    @Test
    void rl111A() {
        rewriteRun(
          preprocessor(getNistResource("RL111A.CBL"))
        );
    }

    @Test
    void rl112A() {
        rewriteRun(
          preprocessor(getNistResource("RL112A.CBL"))
        );
    }

    @Test
    void rl113A() {
        rewriteRun(
          preprocessor(getNistResource("RL113A.CBL"))
        );
    }

    @Test
    void rl114A() {
        rewriteRun(
          preprocessor(getNistResource("RL114A.CBL"))
        );
    }

    @Test
    void rl115A() {
        rewriteRun(
          preprocessor(getNistResource("RL115A.CBL"))
        );
    }

    @Test
    void rl116A() {
        rewriteRun(
          preprocessor(getNistResource("RL116A.CBL"))
        );
    }

    @Test
    void rl117A() {
        rewriteRun(
          preprocessor(getNistResource("RL117A.CBL"))
        );
    }

    @Test
    void rl118A() {
        rewriteRun(
          preprocessor(getNistResource("RL118A.CBL"))
        );
    }

    @Test
    void rl119A() {
        rewriteRun(
          preprocessor(getNistResource("RL119A.CBL"))
        );
    }

    @Test
    void rl201A() {
        rewriteRun(
          preprocessor(getNistResource("RL201A.CBL"))
        );
    }

    @Test
    void rl202A() {
        rewriteRun(
          preprocessor(getNistResource("RL202A.CBL"))
        );
    }

    @Test
    void rl203A() {
        rewriteRun(
          preprocessor(getNistResource("RL203A.CBL"))
        );
    }

    @Test
    void rl204A() {
        rewriteRun(
          preprocessor(getNistResource("RL204A.CBL"))
        );
    }

    @Test
    void rl205A() {
        rewriteRun(
          preprocessor(getNistResource("RL205A.CBL"))
        );
    }

    @Test
    void rl206A() {
        rewriteRun(
          preprocessor(getNistResource("RL206A.CBL"))
        );
    }

    @Test
    void rl207A() {
        rewriteRun(
          preprocessor(getNistResource("RL207A.CBL"))
        );
    }

    @Test
    void rl208A() {
        rewriteRun(
          preprocessor(getNistResource("RL208A.CBL"))
        );
    }

    @Test
    void rl209A() {
        rewriteRun(
          preprocessor(getNistResource("RL209A.CBL"))
        );
    }

    @Test
    void rl210A() {
        rewriteRun(
          preprocessor(getNistResource("RL210A.CBL"))
        );
    }

    @Test
    void rl211A() {
        rewriteRun(
          preprocessor(getNistResource("RL211A.CBL"))
        );
    }

    @Test
    void rl212A() {
        rewriteRun(
          preprocessor(getNistResource("RL212A.CBL"))
        );
    }

    @Test
    void rl213A() {
        rewriteRun(
          preprocessor(getNistResource("RL213A.CBL"))
        );
    }

    @Test
    void rl301M() {
        rewriteRun(
          preprocessor(getNistResource("RL301M.CBL"))
        );
    }

    @Test
    void rl302M() {
        rewriteRun(
          preprocessor(getNistResource("RL302M.CBL"))
        );
    }

    @Test
    void rl401M() {
        rewriteRun(
          preprocessor(getNistResource("RL401M.CBL"))
        );
    }

    @Test
    void rw101A() {
        rewriteRun(
          preprocessor(getNistResource("RW101A.CBL"))
        );
    }

    @Test
    void rw102A() {
        rewriteRun(
          preprocessor(getNistResource("RW102A.CBL"))
        );
    }

    @Test
    void rw103A() {
        rewriteRun(
          preprocessor(getNistResource("RW103A.CBL"))
        );
    }

    @Test
    void rw104A() {
        rewriteRun(
          preprocessor(getNistResource("RW104A.CBL"))
        );
    }

    @Test
    void rw301M() {
        rewriteRun(
          preprocessor(getNistResource("RW301M.CBL"))
        );
    }

    @Test
    void rw302M() {
        rewriteRun(
          preprocessor(getNistResource("RW302M.CBL"))
        );
    }

    @Test
    void sg101A() {
        rewriteRun(
          preprocessor(getNistResource("SG101A.CBL"))
        );
    }

    @Test
    void sg102A() {
        rewriteRun(
          preprocessor(getNistResource("SG102A.CBL"))
        );
    }

    @Test
    void sg103A() {
        rewriteRun(
          preprocessor(getNistResource("SG103A.CBL"))
        );
    }

    @Test
    void sg104A() {
        rewriteRun(
          preprocessor(getNistResource("SG104A.CBL"))
        );
    }

    @Test
    void sg105A() {
        rewriteRun(
          preprocessor(getNistResource("SG105A.CBL"))
        );
    }

    @Test
    void sg106A() {
        rewriteRun(
          preprocessor(getNistResource("SG106A.CBL"))
        );
    }

    @Test
    void sg201A() {
        rewriteRun(
          preprocessor(getNistResource("SG201A.CBL"))
        );
    }

    @Test
    void sg202A() {
        rewriteRun(
          preprocessor(getNistResource("SG202A.CBL"))
        );
    }

    @Test
    void sg203A() {
        rewriteRun(
          preprocessor(getNistResource("SG203A.CBL"))
        );
    }

    @Test
    void sg204A() {
        rewriteRun(
          preprocessor(getNistResource("SG204A.CBL"))
        );
    }

    @Test
    void sm102A() {
        rewriteRun(
          preprocessor(getNistResource("SM102A.CBL"))
        );
    }

    @Test
    void sm104A() {
        rewriteRun(
          preprocessor(getNistResource("SM104A.CBL"))
        );
    }

    @Test
    void sm204A() {
        rewriteRun(
          preprocessor(getNistResource("SM204A.CBL"))
        );
    }

    @Test
    void sq101M() {
        rewriteRun(
          preprocessor(getNistResource("SQ101M.CBL"))
        );
    }

    @Test
    void sq102A() {
        rewriteRun(
          preprocessor(getNistResource("SQ102A.CBL"))
        );
    }

    @Test
    void sq103A() {
        rewriteRun(
          preprocessor(getNistResource("SQ103A.CBL"))
        );
    }

    @Test
    void sq104A() {
        rewriteRun(
          preprocessor(getNistResource("SQ104A.CBL"))
        );
    }

    @Test
    void sq105A() {
        rewriteRun(
          preprocessor(getNistResource("SQ105A.CBL"))
        );
    }

    @Test
    void sq106A() {
        rewriteRun(
          preprocessor(getNistResource("SQ106A.CBL"))
        );
    }

    @Test
    void sq107A() {
        rewriteRun(
          preprocessor(getNistResource("SQ107A.CBL"))
        );
    }

    @Test
    void sq108A() {
        rewriteRun(
          preprocessor(getNistResource("SQ108A.CBL"))
        );
    }

    @Test
    void sq109M() {
        rewriteRun(
          preprocessor(getNistResource("SQ109M.CBL"))
        );
    }

    @Test
    void sq110M() {
        rewriteRun(
          preprocessor(getNistResource("SQ110M.CBL"))
        );
    }

    @Test
    void sq111A() {
        rewriteRun(
          preprocessor(getNistResource("SQ111A.CBL"))
        );
    }

    @Test
    void sq112A() {
        rewriteRun(
          preprocessor(getNistResource("SQ112A.CBL"))
        );
    }

    @Test
    void sq113A() {
        rewriteRun(
          preprocessor(getNistResource("SQ113A.CBL"))
        );
    }

    @Test
    void sq114A() {
        rewriteRun(
          preprocessor(getNistResource("SQ114A.CBL"))
        );
    }

    @Test
    void sq115A() {
        rewriteRun(
          preprocessor(getNistResource("SQ115A.CBL"))
        );
    }

    @Test
    void sq116A() {
        rewriteRun(
          preprocessor(getNistResource("SQ116A.CBL"))
        );
    }

    @Test
    void sq117A() {
        rewriteRun(
          preprocessor(getNistResource("SQ117A.CBL"))
        );
    }

    @Test
    void sq121A() {
        rewriteRun(
          preprocessor(getNistResource("SQ121A.CBL"))
        );
    }

    @Test
    void sq122A() {
        rewriteRun(
          preprocessor(getNistResource("SQ122A.CBL"))
        );
    }

    @Test
    void sq123A() {
        rewriteRun(
          preprocessor(getNistResource("SQ123A.CBL"))
        );
    }

    @Test
    void sq124A() {
        rewriteRun(
          preprocessor(getNistResource("SQ124A.CBL"))
        );
    }

    @Test
    void sq125A() {
        rewriteRun(
          preprocessor(getNistResource("SQ125A.CBL"))
        );
    }

    @Test
    void sq126A() {
        rewriteRun(
          preprocessor(getNistResource("SQ126A.CBL"))
        );
    }

    @Test
    void sq127A() {
        rewriteRun(
          preprocessor(getNistResource("SQ127A.CBL"))
        );
    }

    @Test
    void sq128A() {
        rewriteRun(
          preprocessor(getNistResource("SQ128A.CBL"))
        );
    }

    @Test
    void sq129A() {
        rewriteRun(
          preprocessor(getNistResource("SQ129A.CBL"))
        );
    }

    @Test
    void sq130A() {
        rewriteRun(
          preprocessor(getNistResource("SQ130A.CBL"))
        );
    }

    @Test
    void sq131A() {
        rewriteRun(
          preprocessor(getNistResource("SQ131A.CBL"))
        );
    }

    @Test
    void sq132A() {
        rewriteRun(
          preprocessor(getNistResource("SQ132A.CBL"))
        );
    }

    @Test
    void sq133A() {
        rewriteRun(
          preprocessor(getNistResource("SQ133A.CBL"))
        );
    }

    @Test
    void sq134A() {
        rewriteRun(
          preprocessor(getNistResource("SQ134A.CBL"))
        );
    }

    @Test
    void sq135A() {
        rewriteRun(
          preprocessor(getNistResource("SQ135A.CBL"))
        );
    }

    @Test
    void sq136A() {
        rewriteRun(
          preprocessor(getNistResource("SQ136A.CBL"))
        );
    }

    @Test
    void sq137A() {
        rewriteRun(
          preprocessor(getNistResource("SQ137A.CBL"))
        );
    }

    @Test
    void sq138A() {
        rewriteRun(
          preprocessor(getNistResource("SQ138A.CBL"))
        );
    }

    @Test
    void sq139A() {
        rewriteRun(
          preprocessor(getNistResource("SQ139A.CBL"))
        );
    }

    @Test
    void sq140A() {
        rewriteRun(
          preprocessor(getNistResource("SQ140A.CBL"))
        );
    }

    @Test
    void sq141A() {
        rewriteRun(
          preprocessor(getNistResource("SQ141A.CBL"))
        );
    }

    @Test
    void sq142A() {
        rewriteRun(
          preprocessor(getNistResource("SQ142A.CBL"))
        );
    }

    @Test
    void sq143A() {
        rewriteRun(
          preprocessor(getNistResource("SQ143A.CBL"))
        );
    }

    @Test
    void sq144A() {
        rewriteRun(
          preprocessor(getNistResource("SQ144A.CBL"))
        );
    }

    @Test
    void sq146A() {
        rewriteRun(
          preprocessor(getNistResource("SQ146A.CBL"))
        );
    }

    @Test
    void sq147A() {
        rewriteRun(
          preprocessor(getNistResource("SQ147A.CBL"))
        );
    }

    @Test
    void sq148A() {
        rewriteRun(
          preprocessor(getNistResource("SQ148A.CBL"))
        );
    }

    @Test
    void sq149A() {
        rewriteRun(
          preprocessor(getNistResource("SQ149A.CBL"))
        );
    }

    @Test
    void sq150A() {
        rewriteRun(
          preprocessor(getNistResource("SQ150A.CBL"))
        );
    }

    @Test
    void sq151A() {
        rewriteRun(
          preprocessor(getNistResource("SQ151A.CBL"))
        );
    }

    @Test
    void sq152A() {
        rewriteRun(
          preprocessor(getNistResource("SQ152A.CBL"))
        );
    }

    @Test
    void sq153A() {
        rewriteRun(
          preprocessor(getNistResource("SQ153A.CBL"))
        );
    }

    @Test
    void sq154A() {
        rewriteRun(
          preprocessor(getNistResource("SQ154A.CBL"))
        );
    }

    @Test
    void sq155A() {
        rewriteRun(
          preprocessor(getNistResource("SQ155A.CBL"))
        );
    }

    @Test
    void sq156A() {
        rewriteRun(
          preprocessor(getNistResource("SQ156A.CBL"))
        );
    }

    @Test
    void sq201M() {
        rewriteRun(
          preprocessor(getNistResource("SQ201M.CBL"))
        );
    }

    @Test
    void sq202A() {
        rewriteRun(
          preprocessor(getNistResource("SQ202A.CBL"))
        );
    }

    @Test
    void sq203A() {
        rewriteRun(
          preprocessor(getNistResource("SQ203A.CBL"))
        );
    }

    @Test
    void sq204A() {
        rewriteRun(
          preprocessor(getNistResource("SQ204A.CBL"))
        );
    }

    @Test
    void sq205A() {
        rewriteRun(
          preprocessor(getNistResource("SQ205A.CBL"))
        );
    }

    @Test
    void sq206A() {
        rewriteRun(
          preprocessor(getNistResource("SQ206A.CBL"))
        );
    }

    @Test
    void sq207M() {
        rewriteRun(
          preprocessor(getNistResource("SQ207M.CBL"))
        );
    }

    @Test
    void sq208M() {
        rewriteRun(
          preprocessor(getNistResource("SQ208M.CBL"))
        );
    }

    @Test
    void sq209M() {
        rewriteRun(
          preprocessor(getNistResource("SQ209M.CBL"))
        );
    }

    @Test
    void sq210M() {
        rewriteRun(
          preprocessor(getNistResource("SQ210M.CBL"))
        );
    }

    @Test
    void sq211A() {
        rewriteRun(
          preprocessor(getNistResource("SQ211A.CBL"))
        );
    }

    @Test
    void sq212A() {
        rewriteRun(
          preprocessor(getNistResource("SQ212A.CBL"))
        );
    }

    @Test
    void sq213A() {
        rewriteRun(
          preprocessor(getNistResource("SQ213A.CBL"))
        );
    }

    @Test
    void sq214A() {
        rewriteRun(
          preprocessor(getNistResource("SQ214A.CBL"))
        );
    }

    @Test
    void sq215A() {
        rewriteRun(
          preprocessor(getNistResource("SQ215A.CBL"))
        );
    }

    @Test
    void sq216A() {
        rewriteRun(
          preprocessor(getNistResource("SQ216A.CBL"))
        );
    }

    @Test
    void sq217A() {
        rewriteRun(
          preprocessor(getNistResource("SQ217A.CBL"))
        );
    }

    @Test
    void sq218A() {
        rewriteRun(
          preprocessor(getNistResource("SQ218A.CBL"))
        );
    }

    @Test
    void sq219A() {
        rewriteRun(
          preprocessor(getNistResource("SQ219A.CBL"))
        );
    }

    @Test
    void sq220A() {
        rewriteRun(
          preprocessor(getNistResource("SQ220A.CBL"))
        );
    }

    @Test
    void sq221A() {
        rewriteRun(
          preprocessor(getNistResource("SQ221A.CBL"))
        );
    }

    @Test
    void sq222A() {
        rewriteRun(
          preprocessor(getNistResource("SQ222A.CBL"))
        );
    }

    @Test
    void sq223A() {
        rewriteRun(
          preprocessor(getNistResource("SQ223A.CBL"))
        );
    }

    @Test
    void sq224A() {
        rewriteRun(
          preprocessor(getNistResource("SQ224A.CBL"))
        );
    }

    @Test
    void sq225A() {
        rewriteRun(
          preprocessor(getNistResource("SQ225A.CBL"))
        );
    }

    @Test
    void sq226A() {
        rewriteRun(
          preprocessor(getNistResource("SQ226A.CBL"))
        );
    }

    @Test
    void sq227A() {
        rewriteRun(
          preprocessor(getNistResource("SQ227A.CBL"))
        );
    }

    @Test
    void sq228A() {
        rewriteRun(
          preprocessor(getNistResource("SQ228A.CBL"))
        );
    }

    @Test
    void sq229A() {
        rewriteRun(
          preprocessor(getNistResource("SQ229A.CBL"))
        );
    }

    @Test
    void sq230A() {
        rewriteRun(
          preprocessor(getNistResource("SQ230A.CBL"))
        );
    }

    @Test
    void sq302M() {
        rewriteRun(
          preprocessor(getNistResource("SQ302M.CBL"))
        );
    }

    @Test
    void sq303M() {
        rewriteRun(
          preprocessor(getNistResource("SQ303M.CBL"))
        );
    }

    @Test
    void sq401M() {
        rewriteRun(
          preprocessor(getNistResource("SQ401M.CBL"))
        );
    }

    @Test
    void st101A() {
        rewriteRun(
          preprocessor(getNistResource("ST101A.CBL"))
        );
    }

    @Test
    void st102A() {
        rewriteRun(
          preprocessor(getNistResource("ST102A.CBL"))
        );
    }

    @Test
    void st103A() {
        rewriteRun(
          preprocessor(getNistResource("ST103A.CBL"))
        );
    }

    @Test
    void st104A() {
        rewriteRun(
          preprocessor(getNistResource("ST104A.CBL"))
        );
    }

    @Test
    void st105A() {
        rewriteRun(
          preprocessor(getNistResource("ST105A.CBL"))
        );
    }

    @Test
    void st106A() {
        rewriteRun(
          preprocessor(getNistResource("ST106A.CBL"))
        );
    }

    @Test
    void st107A() {
        rewriteRun(
          preprocessor(getNistResource("ST107A.CBL"))
        );
    }

    @Test
    void st108A() {
        rewriteRun(
          preprocessor(getNistResource("ST108A.CBL"))
        );
    }

    @Test
    void st109A() {
        rewriteRun(
          preprocessor(getNistResource("ST109A.CBL"))
        );
    }

    @Test
    void st110A() {
        rewriteRun(
          preprocessor(getNistResource("ST110A.CBL"))
        );
    }

    @Test
    void st111A() {
        rewriteRun(
          preprocessor(getNistResource("ST111A.CBL"))
        );
    }

    @Test
    void st112M() {
        rewriteRun(
          preprocessor(getNistResource("ST112M.CBL"))
        );
    }

    @Test
    void st113M() {
        rewriteRun(
          preprocessor(getNistResource("ST113M.CBL"))
        );
    }

    @Test
    void st114M() {
        rewriteRun(
          preprocessor(getNistResource("ST114M.CBL"))
        );
    }

    @Test
    void st115A() {
        rewriteRun(
          preprocessor(getNistResource("ST115A.CBL"))
        );
    }

    @Test
    void st116A() {
        rewriteRun(
          preprocessor(getNistResource("ST116A.CBL"))
        );
    }

    @Test
    void st117A() {
        rewriteRun(
          preprocessor(getNistResource("ST117A.CBL"))
        );
    }

    @Test
    void st118A() {
        rewriteRun(
          preprocessor(getNistResource("ST118A.CBL"))
        );
    }

    @Test
    void st119A() {
        rewriteRun(
          preprocessor(getNistResource("ST119A.CBL"))
        );
    }

    @Test
    void st120A() {
        rewriteRun(
          preprocessor(getNistResource("ST120A.CBL"))
        );
    }

    @Test
    void st121A() {
        rewriteRun(
          preprocessor(getNistResource("ST121A.CBL"))
        );
    }

    @Test
    void st122A() {
        rewriteRun(
          preprocessor(getNistResource("ST122A.CBL"))
        );
    }

    @Test
    void st123A() {
        rewriteRun(
          preprocessor(getNistResource("ST123A.CBL"))
        );
    }

    @Test
    void st124A() {
        rewriteRun(
          preprocessor(getNistResource("ST124A.CBL"))
        );
    }

    @Test
    void st125A() {
        rewriteRun(
          preprocessor(getNistResource("ST125A.CBL"))
        );
    }

    @Test
    void st126A() {
        rewriteRun(
          preprocessor(getNistResource("ST126A.CBL"))
        );
    }

    @Test
    void st127A() {
        rewriteRun(
          preprocessor(getNistResource("ST127A.CBL"))
        );
    }

    @Test
    void st131A() {
        rewriteRun(
          preprocessor(getNistResource("ST131A.CBL"))
        );
    }

    @Test
    void st132A() {
        rewriteRun(
          preprocessor(getNistResource("ST132A.CBL"))
        );
    }

    @Test
    void st133A() {
        rewriteRun(
          preprocessor(getNistResource("ST133A.CBL"))
        );
    }

    @Test
    void st134A() {
        rewriteRun(
          preprocessor(getNistResource("ST134A.CBL"))
        );
    }

    @Test
    void st135A() {
        rewriteRun(
          preprocessor(getNistResource("ST135A.CBL"))
        );
    }

    @Test
    void st136A() {
        rewriteRun(
          preprocessor(getNistResource("ST136A.CBL"))
        );
    }

    @Test
    void st137A() {
        rewriteRun(
          preprocessor(getNistResource("ST137A.CBL"))
        );
    }

    @Test
    void st139A() {
        rewriteRun(
          preprocessor(getNistResource("ST139A.CBL"))
        );
    }

    @Test
    void st140A() {
        rewriteRun(
          preprocessor(getNistResource("ST140A.CBL"))
        );
    }

    @Test
    void st144A() {
        rewriteRun(
          preprocessor(getNistResource("ST144A.CBL"))
        );
    }

    @Test
    void st146A() {
        rewriteRun(
          preprocessor(getNistResource("ST146A.CBL"))
        );
    }

    @Test
    void st147A() {
        rewriteRun(
          preprocessor(getNistResource("ST147A.CBL"))
        );
    }

    @Test
    void st301M() {
        rewriteRun(
          preprocessor(getNistResource("ST301M.CBL"))
        );
    }
}
