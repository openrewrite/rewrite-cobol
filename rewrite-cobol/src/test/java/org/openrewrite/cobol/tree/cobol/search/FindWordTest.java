/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.cobol.tree.cobol.search;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.openrewrite.Tree;
import org.openrewrite.TreeVisitor;
import org.openrewrite.cobol.CobolTest;
import org.openrewrite.cobol.search.FindWord;
import org.openrewrite.marker.Marker;
import org.openrewrite.marker.SearchResult;
import org.openrewrite.test.RecipeSpec;

import java.util.ArrayList;
import java.util.List;

import static org.openrewrite.cobol.Assertions.cobol;

public class FindWordTest extends CobolTest {
    @Override
    public void defaults(RecipeSpec spec) {
        spec.recipe(new FindWord("CM102M", true));
    }

    private final TreeVisitor<Tree, List<SearchResult>> visitor = new TreeVisitor<>() {
        @Override
        public <M extends Marker> M visitMarker(Marker marker, List<SearchResult> p) {
            if (marker instanceof SearchResult) {
                p.add((SearchResult) marker);
            }
            return super.visitMarker(marker, p);
        }
    };

    @Test
    void wordIsNotUsed() {
        rewriteRun(
          cobol(getNistResource("DB101A.CBL"), true)
        );
    }

    @Test
    void cm102mExactMatch() {
        rewriteRun(
          cobol(
            """
              000100 IDENTIFICATION DIVISION.                                         CM1024.2
              000200 PROGRAM-ID.                                                      CM1024.2
              000300     CM102M.                                                      CM1024.2
              000400 AUTHOR.                                                          CM1024.2
              000500     FEDERAL COMPILER TESTING CENTER.                             CM1024.2
              000600 INSTALLATION.                                                    CM1024.2
              000700     GENERAL SERVICES ADMINISTRATION                              CM1024.2
              000800     AUTOMATED DATA AND TELECOMMUNICATION SERVICE.                CM1024.2
              000900     SOFTWARE DEVELOPMENT OFFICE.                                 CM1024.2
              001000     5203 LEESBURG PIKE  SUITE 1100                               CM1024.2
              001100     FALLS CHURCH VIRGINIA 22041.                                 CM1024.2
              001200                                                                  CM1024.2
              001300     PHONE   (703) 756-6153                                       CM1024.2
              001400                                                                  CM1024.2
              001500     " HIGH       ".                                              CM1024.2
              """,
            """
              000100 IDENTIFICATION DIVISION.                                         CM1024.2
              000200 PROGRAM-ID.                                                      CM1024.2
              000300     ~~>CM102M.                                                      CM1024.2
              000400 AUTHOR.                                                          CM1024.2
              000500     FEDERAL COMPILER TESTING CENTER.                             CM1024.2
              000600 INSTALLATION.                                                    CM1024.2
              000700     GENERAL SERVICES ADMINISTRATION                              CM1024.2
              000800     AUTOMATED DATA AND TELECOMMUNICATION SERVICE.                CM1024.2
              000900     SOFTWARE DEVELOPMENT OFFICE.                                 CM1024.2
              001000     5203 LEESBURG PIKE  SUITE 1100                               CM1024.2
              001100     FALLS CHURCH VIRGINIA 22041.                                 CM1024.2
              001200                                                                  CM1024.2
              001300     PHONE   (703) 756-6153                                       CM1024.2
              001400                                                                  CM1024.2
              001500     " HIGH       ".                                              CM1024.2
              """,
            spec -> spec.afterRecipe(cu -> {
                List<SearchResult> searchResults = new ArrayList<>(1);
                visitor.visit(cu, searchResults);
                Assertions.assertThat(searchResults).hasSize(1);
            }), true)
        );
    }

    @Test
    void cm102mPartialMatch() {
        rewriteRun(
          spec -> spec.recipe(new FindWord("cm.*", false)),
          cobol(
            """
              000100 IDENTIFICATION DIVISION.                                         CM1024.2
              000200 PROGRAM-ID.                                                      CM1024.2
              000300     CM102M.                                                      CM1024.2
              003200 DATA DIVISION.                                                   CM1024.2
              027700 COMMUNICATION SECTION.                                           CM1024.2
              027800 CD  CM-OUTQUE-1 FOR OUTPUT                                       CM1024.2
              027900     DESTINATION COUNT IS ONE                                     CM1024.2
              028000     TEXT LENGTH IS MSG-LENGTH                                    CM1024.2
              028100     STATUS KEY IS STATUS-KEY                                     CM1024.2
              028200     ERROR KEY IS ERR-KEY                                         CM1024.2
              028300     SYMBOLIC DESTINATION IS SYM-DEST.                            CM1024.2
              028400 PROCEDURE    DIVISION.                                           CM1024.2
              028500 SECT-CM102M-0001 SECTION.                                        CM1024.2
              028600 CM102M-INIT.                                                     CM1024.2
              028700     OPEN     OUTPUT PRINT-FILE.                                  CM1024.2
              028800     MOVE "CM102M     " TO TEST-ID.                               CM1024.2
              028900     MOVE     TEST-ID TO ID-AGAIN.                                CM1024.2
              029000     MOVE    SPACE TO TEST-RESULTS.                               CM1024.2
              029100     PERFORM HEAD-ROUTINE.                                        CM1024.2
              029200     PERFORM COLUMN-NAMES-ROUTINE.                                CM1024.2
              029300     MOVE "MCS STATUS WORD" TO FEATURE.                           CM1024.2
              029400 DISAB-STATUS-TEST-01.                                            CM1024.2
              029500     MOVE "INITIAL DISABLE TO OUTPUT CD" TO RE-MARK.              CM1024.2
              029600     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
              029700     MOVE 1 TO ONE.                                               CM1024.2
              029800     MOVE                                                         CM1024.2
              029900     XXXXX032                                                     CM1024.2
              030000         TO SYM-DEST.                                             CM1024.2
              030100     DISABLE OUTPUT CM-OUTQUE-1 WITH KEY                          CM1024.2
              030200     XXXXX033.                                                    CM1024.2
              030300     MOVE "INFO" TO P-OR-F.                                       CM1024.2
              030400     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
              030500     MOVE "/" TO SLASH.                                           CM1024.2
              030600     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
              030700     MOVE "       INFO TEST FOR" TO CORRECT-A.                    CM1024.2
              030800     GO TO DISAB-STATUS-WRITE-01.                                 CM1024.2
              031400 DISAB-STATUS-TEST-02.                                            CM1024.2
              031500     MOVE "NO DESTINATION SPECIFIED" TO RE-MARK.                  CM1024.2
              031600     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
              031700     MOVE "GARBAGE" TO SYM-DEST.                                  CM1024.2
              031800     MOVE 1 TO ONE.                                               CM1024.2
              031900     DISABLE OUTPUT CM-OUTQUE-1 WITH KEY                          CM1024.2
              032000     XXXXX033.                                                    CM1024.2
              032100     IF STATUS-KEY IS EQUAL TO "20"                               CM1024.2
              032200         AND ERR-KEY IS EQUAL TO "1"                              CM1024.2
              032300         PERFORM PASS GO TO DISAB-STATUS-WRITE-02.                CM1024.2
              032400     MOVE 201 TO CORRECT-2SLASH1.                                 CM1024.2
              032500     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
              032600     MOVE "/" TO SLASH.                                           CM1024.2
              032700     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
              032800     PERFORM FAIL.                                                CM1024.2
              032900     GO TO DISAB-STATUS-WRITE-02.                                 CM1024.2
              033500 DISAB-STATUS-TEST-03.                                            CM1024.2
              033600     MOVE "INVALID PASSWORD USED" TO RE-MARK.                     CM1024.2
              033700     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
              033800     MOVE 1 TO ONE.                                               CM1024.2
              033900     MOVE                                                         CM1024.2
              034000     XXXXX032                                                     CM1024.2
              034100         TO SYM-DEST.                                             CM1024.2
              034200     DISABLE OUTPUT CM-OUTQUE-1 WITH KEY                          CM1024.2
              034300         "GARBAGE".                                               CM1024.2
              034400     IF STATUS-KEY IS EQUAL TO "40"                               CM1024.2
              034500         PERFORM PASS GO TO DISAB-STATUS-WRITE-03.                CM1024.2
              034600     MOVE 400 TO CORRECT-2SLASH1.                                 CM1024.2
              034700     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
              034800     MOVE "/" TO SLASH.                                           CM1024.2
              034900     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
              035000     PERFORM FAIL.                                                CM1024.2
              035100     GO TO DISAB-STATUS-WRITE-03.                                 CM1024.2
              035700 DISAB-STATUS-TEST-04.                                            CM1024.2
              035800     MOVE "INVALID DESTINATION COUNT (0)" TO RE-MARK.             CM1024.2
              035900     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
              036000     MOVE                                                         CM1024.2
              036100     XXXXX032                                                     CM1024.2
              036200         TO SYM-DEST.                                             CM1024.2
              036300     MOVE 0 TO ONE.                                               CM1024.2
              036400     DISABLE OUTPUT CM-OUTQUE-1 WITH KEY                          CM1024.2
              036500     XXXXX033.                                                    CM1024.2
              036600     IF STATUS-KEY IS EQUAL TO "30"                               CM1024.2
              036700         PERFORM PASS GO TO DISAB-STATUS-WRITE-04.                CM1024.2
              036800     MOVE 300 TO CORRECT-2SLASH1.                                 CM1024.2
              036900     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
              037000     MOVE "/" TO SLASH.                                           CM1024.2
              037100     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
              037200     PERFORM FAIL.                                                CM1024.2
              037300     GO TO DISAB-STATUS-WRITE-04.                                 CM1024.2
              037900 DISAB-STATUS-TEST-05.                                            CM1024.2
              038000     MOVE "COMBINATION ERROR" TO RE-MARK.                         CM1024.2
              038100     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
              038200     MOVE SPACES TO SYM-DEST.                                     CM1024.2
              038300     MOVE 0 TO ONE.                                               CM1024.2
              038400     DISABLE OUTPUT CM-OUTQUE-1 WITH KEY                          CM1024.2
              038500         "GARBAGE".                                               CM1024.2
              038600     MOVE "INFO" TO P-OR-F.                                       CM1024.2
              038700     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
              038800     MOVE "/" TO SLASH.                                           CM1024.2
              038900     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
              039000     GO TO DISAB-STATUS-WRITE-05.                                 CM1024.2
              039600 SEND-STATUS-TEST-01.                                             CM1024.2
              039700     MOVE "DESTINATION DISABLED" TO RE-MARK.                      CM1024.2
              039800     MOVE "CM102M- I AM THE FIRST MESSAGE IN QUEUE;" TO MSG-70.   CM1024.2
              039900     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
              040000     MOVE                                                         CM1024.2
              040100     XXXXX032                                                     CM1024.2
              040200         TO SYM-DEST.                                             CM1024.2
              040300     MOVE 1 TO ONE.                                               CM1024.2
              040400     MOVE 45 TO MSG-LENGTH.                                       CM1024.2
              040500     SEND CM-OUTQUE-1 FROM MSG-70 WITH EMI                        CM1024.2
              040600         AFTER ADVANCING PAGE.                                    CM1024.2
              040700     MOVE "THOU SHALT HAVE NO OTHER MESSAGES BEFORE ME." TO MSG-70CM1024.2
              040800     SEND CM-OUTQUE-1 FROM MSG-70 WITH EMI.                       CM1024.2
              040900     MOVE SPACES TO MSG-70.                                       CM1024.2
              041000     MOVE 1 TO MSG-LENGTH.                                        CM1024.2
              041100     SEND CM-OUTQUE-1 FROM MSG-70 WITH EGI.                       CM1024.2
              041200     IF STATUS-KEY IS EQUAL TO "10"                               CM1024.2
              041300         PERFORM PASS GO TO SEND-STATUS-WRITE-01.                 CM1024.2
              041400     MOVE 100 TO CORRECT-2SLASH1.                                 CM1024.2
              041500     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
              041600     MOVE "/" TO SLASH.                                           CM1024.2
              041700     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
              041800     PERFORM FAIL.                                                CM1024.2
              041900     GO TO SEND-STATUS-WRITE-01.                                  CM1024.2
              042500 SEND-STATUS-TEST-02.                                             CM1024.2
              042600     MOVE "COMBINATION ERROR" TO RE-MARK.                         CM1024.2
              042700     MOVE SPACES TO SYM-DEST.                                     CM1024.2
              042800     MOVE 0 TO ONE.                                               CM1024.2
              042900     MOVE 100 TO MSG-LENGTH.                                      CM1024.2
              043000     MOVE "S-02" TO TEST-IND.                                     CM1024.2
              043100     SEND CM-OUTQUE-1 FROM ERR-MSG WITH EMI.                      CM1024.2
              043200     MOVE "INFO" TO P-OR-F.                                       CM1024.2
              043300     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
              043400     MOVE "/" TO SLASH.                                           CM1024.2
              043500     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
              043600     GO TO SEND-STATUS-WRITE-02.                                  CM1024.2
              044200 ENABL-STATUS-TEST-01.                                            CM1024.2
              044300     MOVE "DESTINATION NOT SPECIFIED" TO RE-MARK.                 CM1024.2
              044400     MOVE SPACES TO SYM-DEST.                                     CM1024.2
              044500     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
              044600     MOVE 1 TO ONE.                                               CM1024.2
              044700     ENABLE OUTPUT CM-OUTQUE-1 WITH KEY                           CM1024.2
              044800     XXXXX033.                                                    CM1024.2
              044900     IF STATUS-KEY IS EQUAL TO "20"                               CM1024.2
              045000         AND ERR-KEY IS EQUAL TO "1"                              CM1024.2
              045100         PERFORM PASS GO TO ENABL-STATUS-WRITE-01.                CM1024.2
              045200     MOVE 201 TO CORRECT-2SLASH1.                                 CM1024.2
              045300     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
              045400     MOVE "/" TO SLASH.                                           CM1024.2
              045500     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
              045600     PERFORM FAIL.                                                CM1024.2
              045700     GO TO ENABL-STATUS-WRITE-01.                                 CM1024.2
              046300 ENABL-STATUS-TEST-02.                                            CM1024.2
              046400     MOVE "INVALID DESTINATION COUNT (0)" TO RE-MARK.             CM1024.2
              046500     MOVE                                                         CM1024.2
              046600     XXXXX032                                                     CM1024.2
              046700         TO SYM-DEST.                                             CM1024.2
              046800     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
              046900     MOVE 0 TO ONE.                                               CM1024.2
              047000     ENABLE OUTPUT CM-OUTQUE-1 WITH KEY                           CM1024.2
              047100     XXXXX033.                                                    CM1024.2
              047200     IF STATUS-KEY IS EQUAL TO "30"                               CM1024.2
              047300         PERFORM PASS GO TO ENABL-STATUS-WRITE-02.                CM1024.2
              047400     MOVE 300 TO CORRECT-2SLASH1.                                 CM1024.2
              047500     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
              047600     MOVE "/" TO SLASH.                                           CM1024.2
              047700     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
              047800     PERFORM FAIL.                                                CM1024.2
              047900     GO TO ENABL-STATUS-WRITE-02.                                 CM1024.2
              048500 ENABL-STATUS-TEST-03.                                            CM1024.2
              048600     MOVE "INVALID PASSWORD USED" TO RE-MARK.                     CM1024.2
              048700     MOVE                                                         CM1024.2
              048800     XXXXX032                                                     CM1024.2
              048900         TO SYM-DEST.                                             CM1024.2
              049000     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
              049100     MOVE 1 TO ONE.                                               CM1024.2
              049200     ENABLE OUTPUT CM-OUTQUE-1 WITH KEY                           CM1024.2
              049300         "GARBAGE".                                               CM1024.2
              049400     IF STATUS-KEY IS EQUAL TO "40"                               CM1024.2
              049500         PERFORM PASS GO TO ENABL-STATUS-WRITE-03.                CM1024.2
              049600     MOVE 400 TO CORRECT-2SLASH1.                                 CM1024.2
              049700     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
              049800     MOVE "/" TO SLASH.                                           CM1024.2
              049900     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
              050000     PERFORM FAIL.                                                CM1024.2
              050100     GO TO ENABL-STATUS-WRITE-03.                                 CM1024.2
              050700 ENABL-STATUS-TEST-04.                                            CM1024.2
              050800     MOVE "VALID ENABLE/NO ERROR EXPECTED" TO RE-MARK.            CM1024.2
              050900     MOVE                                                         CM1024.2
              051000     XXXXX032                                                     CM1024.2
              051100         TO SYM-DEST.                                             CM1024.2
              051200     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
              051300     MOVE 1 TO ONE.                                               CM1024.2
              051400     ENABLE OUTPUT CM-OUTQUE-1 WITH KEY                           CM1024.2
              051500     XXXXX033.                                                    CM1024.2
              051600     IF STATUS-KEY IS EQUAL TO ZERO                               CM1024.2
              051700         PERFORM PASS GO TO ENABL-STATUS-WRITE-04.                CM1024.2
              051800     MOVE 0 TO CORRECT-2SLASH1.                                   CM1024.2
              051900     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
              052000     MOVE "/" TO SLASH.                                           CM1024.2
              052100     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
              052200     PERFORM FAIL.                                                CM1024.2
              052300     GO TO ENABL-STATUS-WRITE-04.                                 CM1024.2
              052900 SEND-STATUS-TEST-03.                                             CM1024.2
              053000     MOVE "DESTINATION UNKNOWN" TO RE-MARK.                       CM1024.2
              053100     MOVE "GARBAGE" TO SYM-DEST.                                  CM1024.2
              053200     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
              053300     MOVE 1 TO ONE.                                               CM1024.2
              053400     MOVE 37 TO MSG-LENGTH.                                       CM1024.2
              053500     MOVE "S-03" TO TEST-IND.                                     CM1024.2
              053600     SEND CM-OUTQUE-1 FROM ERR-MSG WITH EMI.                      CM1024.2
              053700     IF STATUS-KEY IS EQUAL TO "20"                               CM1024.2
              053800         AND ERR-KEY IS EQUAL TO "1"                              CM1024.2
              053900         PERFORM PASS GO TO SEND-STATUS-WRITE-03.                 CM1024.2
              054000     MOVE 201 TO CORRECT-2SLASH1.                                 CM1024.2
              054100     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
              054200     MOVE "/" TO SLASH.                                           CM1024.2
              054300     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
              054400     PERFORM FAIL.                                                CM1024.2
              054500     GO TO SEND-STATUS-WRITE-03.                                  CM1024.2
              055100 SEND-STATUS-TEST-04.                                             CM1024.2
              055200     MOVE "DESTINATION COUNT INVALID (0)" TO RE-MARK.             CM1024.2
              055300     MOVE                                                         CM1024.2
              055400     XXXXX032                                                     CM1024.2
              055500         TO SYM-DEST.                                             CM1024.2
              055600     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
              055700     MOVE 0 TO ONE.                                               CM1024.2
              055800     MOVE 37 TO MSG-LENGTH.                                       CM1024.2
              055900     MOVE "S-04" TO TEST-IND.                                     CM1024.2
              056000     SEND CM-OUTQUE-1 FROM ERR-MSG WITH EMI.                      CM1024.2
              056100     IF STATUS-KEY IS EQUAL TO "30"                               CM1024.2
              056200         PERFORM PASS GO TO SEND-STATUS-WRITE-04.                 CM1024.2
              056300     MOVE 300 TO CORRECT-2SLASH1.                                 CM1024.2
              056400     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
              056500     MOVE "/" TO SLASH.                                           CM1024.2
              056600     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
              056700     PERFORM FAIL.                                                CM1024.2
              056800     GO TO SEND-STATUS-WRITE-04.                                  CM1024.2
              057400 SEND-STATUS-TEST-05.                                             CM1024.2
              057500     MOVE "CHARACTER COUNT EXCESSIVE" TO RE-MARK.                 CM1024.2
              057600     MOVE                                                         CM1024.2
              057700     XXXXX032                                                     CM1024.2
              057800         TO SYM-DEST.                                             CM1024.2
              057900     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
              058000     MOVE 1 TO ONE.                                               CM1024.2
              058100     MOVE 38 TO MSG-LENGTH.                                       CM1024.2
              058200     MOVE "S-05" TO TEST-IND.                                     CM1024.2
              058300     SEND CM-OUTQUE-1 FROM ERR-MSG WITH EMI.                      CM1024.2
              058400     IF STATUS-KEY IS EQUAL TO "50"                               CM1024.2
              058500         PERFORM PASS GO TO SEND-STATUS-WRITE-05.                 CM1024.2
              058600     MOVE 500 TO CORRECT-2SLASH1.                                 CM1024.2
              058700     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
              058800     MOVE "/" TO SLASH.                                           CM1024.2
              058900     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
              059000     PERFORM FAIL.                                                CM1024.2
              059100     GO TO SEND-STATUS-WRITE-05.                                  CM1024.2
              082200 SEND-EMI-A1.                                                     CM1024.2
              082300     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EMI.                      CM1024.2
              082400     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              082500 SEND-EGI-A1.                                                     CM1024.2
              082600     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EGI.                      CM1024.2
              082700     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              082800 SEND-EMI-AP.                                                     CM1024.2
              082900     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EMI AFTER PAGE.           CM1024.2
              083000     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              083100 SEND-EMI-A3-01.                                                  CM1024.2
              083200     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EMI AFTER ADVANCING 3     CM1024.2
              083300         LINES.                                                   CM1024.2
              083400     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              083500 SEND-EMI-A3-02.                                                  CM1024.2
              083600     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
              083700         AFTER ADVANCING THREE LINES.                             CM1024.2
              083800     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              083900 SEND-EMI-A3-03.                                                  CM1024.2
              084000     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
              084100         AFTER 3 LINE.                                            CM1024.2
              084200     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              084300 SEND-EMI-A3-04.                                                  CM1024.2
              084400     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
              084500         AFTER COMP-THREE.                                        CM1024.2
              084600     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              084700 SEND-EMI-A3-05.                                                  CM1024.2
              084800     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
              084900         AFTER 3.                                                 CM1024.2
              085000     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              085100 SEND-EGI-ONLY.                                                   CM1024.2
              085200     SEND CM-OUTQUE-1 WITH EGI.                                   CM1024.2
              085300     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              085400 SEND-EMI-BP.                                                     CM1024.2
              085500     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
              085600         BEFORE ADVANCING PAGE.                                   CM1024.2
              085700     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              085800 SEND-EMI-B2-01.                                                  CM1024.2
              085900     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
              086000         BEFORE ADVANCING 2 LINES.                                CM1024.2
              086100     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              086200 SEND-EMI-B2-02.                                                  CM1024.2
              086300     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
              086400         BEFORE ADVANCING TWO LINES.                              CM1024.2
              086500     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              086600 SEND-EMI-B2-03.                                                  CM1024.2
              086700     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
              086800         BEFORE 2 LINE.                                           CM1024.2
              086900     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              087000 SEND-EMI-B2-04.                                                  CM1024.2
              087100     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
              087200         BEFORE COMP-TWO.                                         CM1024.2
              087300     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              087400 SEND-EMI-B2-05.                                                  CM1024.2
              087500     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
              087600         BEFORE 2.                                                CM1024.2
              087700     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              087800 SEND-EMI-A0.                                                     CM1024.2
              087900     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
              088000         AFTER 0 LINES.                                           CM1024.2
              088100     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              088200 SEND-EMI-B0.                                                     CM1024.2
              088300     SEND CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
              088400         BEFORE ZERO LINES.                                       CM1024.2
              088500     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              088600 SEND-LONG-MSG.                                                   CM1024.2
              088700     SEND CM-OUTQUE-1 FROM LONG-MSG WITH EMI AFTER PAGE.          CM1024.2
              088800     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              088900 DISABLE-OUTQUE.                                                  CM1024.2
              089000     DISABLE OUTPUT CM-OUTQUE-1 KEY                               CM1024.2
              089100     PASSWORD1.                                                   CM1024.2
              089200     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              089300 ENABLE-OUTQUE.                                                   CM1024.2
              089400     ENABLE OUTPUT CM-OUTQUE-1 WITH KEY                           CM1024.2
              089500     XXXXX033.                                                    CM1024.2
              """,
            """
              000100 IDENTIFICATION DIVISION.                                         CM1024.2
              000200 PROGRAM-ID.                                                      CM1024.2
              000300     ~~>CM102M.                                                      CM1024.2
              003200 DATA DIVISION.                                                   CM1024.2
              027700 COMMUNICATION SECTION.                                           CM1024.2
              027800 CD  ~~>CM-OUTQUE-1 FOR OUTPUT                                       CM1024.2
              027900     DESTINATION COUNT IS ONE                                     CM1024.2
              028000     TEXT LENGTH IS MSG-LENGTH                                    CM1024.2
              028100     STATUS KEY IS STATUS-KEY                                     CM1024.2
              028200     ERROR KEY IS ERR-KEY                                         CM1024.2
              028300     SYMBOLIC DESTINATION IS SYM-DEST.                            CM1024.2
              028400 PROCEDURE    DIVISION.                                           CM1024.2
              028500 SECT-CM102M-0001 SECTION.                                        CM1024.2
              028600 ~~>CM102M-INIT.                                                     CM1024.2
              028700     OPEN     OUTPUT PRINT-FILE.                                  CM1024.2
              028800     MOVE "CM102M     " TO TEST-ID.                               CM1024.2
              028900     MOVE     TEST-ID TO ID-AGAIN.                                CM1024.2
              029000     MOVE    SPACE TO TEST-RESULTS.                               CM1024.2
              029100     PERFORM HEAD-ROUTINE.                                        CM1024.2
              029200     PERFORM COLUMN-NAMES-ROUTINE.                                CM1024.2
              029300     MOVE "MCS STATUS WORD" TO FEATURE.                           CM1024.2
              029400 DISAB-STATUS-TEST-01.                                            CM1024.2
              029500     MOVE "INITIAL DISABLE TO OUTPUT CD" TO RE-MARK.              CM1024.2
              029600     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
              029700     MOVE 1 TO ONE.                                               CM1024.2
              029800     MOVE                                                         CM1024.2
              029900     XXXXX032                                                     CM1024.2
              030000         TO SYM-DEST.                                             CM1024.2
              030100     DISABLE OUTPUT ~~>CM-OUTQUE-1 WITH KEY                          CM1024.2
              030200     XXXXX033.                                                    CM1024.2
              030300     MOVE "INFO" TO P-OR-F.                                       CM1024.2
              030400     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
              030500     MOVE "/" TO SLASH.                                           CM1024.2
              030600     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
              030700     MOVE "       INFO TEST FOR" TO CORRECT-A.                    CM1024.2
              030800     GO TO DISAB-STATUS-WRITE-01.                                 CM1024.2
              031400 DISAB-STATUS-TEST-02.                                            CM1024.2
              031500     MOVE "NO DESTINATION SPECIFIED" TO RE-MARK.                  CM1024.2
              031600     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
              031700     MOVE "GARBAGE" TO SYM-DEST.                                  CM1024.2
              031800     MOVE 1 TO ONE.                                               CM1024.2
              031900     DISABLE OUTPUT ~~>CM-OUTQUE-1 WITH KEY                          CM1024.2
              032000     XXXXX033.                                                    CM1024.2
              032100     IF STATUS-KEY IS EQUAL TO "20"                               CM1024.2
              032200         AND ERR-KEY IS EQUAL TO "1"                              CM1024.2
              032300         PERFORM PASS GO TO DISAB-STATUS-WRITE-02.                CM1024.2
              032400     MOVE 201 TO CORRECT-2SLASH1.                                 CM1024.2
              032500     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
              032600     MOVE "/" TO SLASH.                                           CM1024.2
              032700     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
              032800     PERFORM FAIL.                                                CM1024.2
              032900     GO TO DISAB-STATUS-WRITE-02.                                 CM1024.2
              033500 DISAB-STATUS-TEST-03.                                            CM1024.2
              033600     MOVE "INVALID PASSWORD USED" TO RE-MARK.                     CM1024.2
              033700     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
              033800     MOVE 1 TO ONE.                                               CM1024.2
              033900     MOVE                                                         CM1024.2
              034000     XXXXX032                                                     CM1024.2
              034100         TO SYM-DEST.                                             CM1024.2
              034200     DISABLE OUTPUT ~~>CM-OUTQUE-1 WITH KEY                          CM1024.2
              034300         "GARBAGE".                                               CM1024.2
              034400     IF STATUS-KEY IS EQUAL TO "40"                               CM1024.2
              034500         PERFORM PASS GO TO DISAB-STATUS-WRITE-03.                CM1024.2
              034600     MOVE 400 TO CORRECT-2SLASH1.                                 CM1024.2
              034700     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
              034800     MOVE "/" TO SLASH.                                           CM1024.2
              034900     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
              035000     PERFORM FAIL.                                                CM1024.2
              035100     GO TO DISAB-STATUS-WRITE-03.                                 CM1024.2
              035700 DISAB-STATUS-TEST-04.                                            CM1024.2
              035800     MOVE "INVALID DESTINATION COUNT (0)" TO RE-MARK.             CM1024.2
              035900     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
              036000     MOVE                                                         CM1024.2
              036100     XXXXX032                                                     CM1024.2
              036200         TO SYM-DEST.                                             CM1024.2
              036300     MOVE 0 TO ONE.                                               CM1024.2
              036400     DISABLE OUTPUT ~~>CM-OUTQUE-1 WITH KEY                          CM1024.2
              036500     XXXXX033.                                                    CM1024.2
              036600     IF STATUS-KEY IS EQUAL TO "30"                               CM1024.2
              036700         PERFORM PASS GO TO DISAB-STATUS-WRITE-04.                CM1024.2
              036800     MOVE 300 TO CORRECT-2SLASH1.                                 CM1024.2
              036900     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
              037000     MOVE "/" TO SLASH.                                           CM1024.2
              037100     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
              037200     PERFORM FAIL.                                                CM1024.2
              037300     GO TO DISAB-STATUS-WRITE-04.                                 CM1024.2
              037900 DISAB-STATUS-TEST-05.                                            CM1024.2
              038000     MOVE "COMBINATION ERROR" TO RE-MARK.                         CM1024.2
              038100     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
              038200     MOVE SPACES TO SYM-DEST.                                     CM1024.2
              038300     MOVE 0 TO ONE.                                               CM1024.2
              038400     DISABLE OUTPUT ~~>CM-OUTQUE-1 WITH KEY                          CM1024.2
              038500         "GARBAGE".                                               CM1024.2
              038600     MOVE "INFO" TO P-OR-F.                                       CM1024.2
              038700     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
              038800     MOVE "/" TO SLASH.                                           CM1024.2
              038900     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
              039000     GO TO DISAB-STATUS-WRITE-05.                                 CM1024.2
              039600 SEND-STATUS-TEST-01.                                             CM1024.2
              039700     MOVE "DESTINATION DISABLED" TO RE-MARK.                      CM1024.2
              039800     MOVE "CM102M- I AM THE FIRST MESSAGE IN QUEUE;" TO MSG-70.   CM1024.2
              039900     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
              040000     MOVE                                                         CM1024.2
              040100     XXXXX032                                                     CM1024.2
              040200         TO SYM-DEST.                                             CM1024.2
              040300     MOVE 1 TO ONE.                                               CM1024.2
              040400     MOVE 45 TO MSG-LENGTH.                                       CM1024.2
              040500     SEND ~~>CM-OUTQUE-1 FROM MSG-70 WITH EMI                        CM1024.2
              040600         AFTER ADVANCING PAGE.                                    CM1024.2
              040700     MOVE "THOU SHALT HAVE NO OTHER MESSAGES BEFORE ME." TO MSG-70CM1024.2
              040800     SEND ~~>CM-OUTQUE-1 FROM MSG-70 WITH EMI.                       CM1024.2
              040900     MOVE SPACES TO MSG-70.                                       CM1024.2
              041000     MOVE 1 TO MSG-LENGTH.                                        CM1024.2
              041100     SEND ~~>CM-OUTQUE-1 FROM MSG-70 WITH EGI.                       CM1024.2
              041200     IF STATUS-KEY IS EQUAL TO "10"                               CM1024.2
              041300         PERFORM PASS GO TO SEND-STATUS-WRITE-01.                 CM1024.2
              041400     MOVE 100 TO CORRECT-2SLASH1.                                 CM1024.2
              041500     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
              041600     MOVE "/" TO SLASH.                                           CM1024.2
              041700     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
              041800     PERFORM FAIL.                                                CM1024.2
              041900     GO TO SEND-STATUS-WRITE-01.                                  CM1024.2
              042500 SEND-STATUS-TEST-02.                                             CM1024.2
              042600     MOVE "COMBINATION ERROR" TO RE-MARK.                         CM1024.2
              042700     MOVE SPACES TO SYM-DEST.                                     CM1024.2
              042800     MOVE 0 TO ONE.                                               CM1024.2
              042900     MOVE 100 TO MSG-LENGTH.                                      CM1024.2
              043000     MOVE "S-02" TO TEST-IND.                                     CM1024.2
              043100     SEND ~~>CM-OUTQUE-1 FROM ERR-MSG WITH EMI.                      CM1024.2
              043200     MOVE "INFO" TO P-OR-F.                                       CM1024.2
              043300     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
              043400     MOVE "/" TO SLASH.                                           CM1024.2
              043500     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
              043600     GO TO SEND-STATUS-WRITE-02.                                  CM1024.2
              044200 ENABL-STATUS-TEST-01.                                            CM1024.2
              044300     MOVE "DESTINATION NOT SPECIFIED" TO RE-MARK.                 CM1024.2
              044400     MOVE SPACES TO SYM-DEST.                                     CM1024.2
              044500     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
              044600     MOVE 1 TO ONE.                                               CM1024.2
              044700     ENABLE OUTPUT ~~>CM-OUTQUE-1 WITH KEY                           CM1024.2
              044800     XXXXX033.                                                    CM1024.2
              044900     IF STATUS-KEY IS EQUAL TO "20"                               CM1024.2
              045000         AND ERR-KEY IS EQUAL TO "1"                              CM1024.2
              045100         PERFORM PASS GO TO ENABL-STATUS-WRITE-01.                CM1024.2
              045200     MOVE 201 TO CORRECT-2SLASH1.                                 CM1024.2
              045300     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
              045400     MOVE "/" TO SLASH.                                           CM1024.2
              045500     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
              045600     PERFORM FAIL.                                                CM1024.2
              045700     GO TO ENABL-STATUS-WRITE-01.                                 CM1024.2
              046300 ENABL-STATUS-TEST-02.                                            CM1024.2
              046400     MOVE "INVALID DESTINATION COUNT (0)" TO RE-MARK.             CM1024.2
              046500     MOVE                                                         CM1024.2
              046600     XXXXX032                                                     CM1024.2
              046700         TO SYM-DEST.                                             CM1024.2
              046800     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
              046900     MOVE 0 TO ONE.                                               CM1024.2
              047000     ENABLE OUTPUT ~~>CM-OUTQUE-1 WITH KEY                           CM1024.2
              047100     XXXXX033.                                                    CM1024.2
              047200     IF STATUS-KEY IS EQUAL TO "30"                               CM1024.2
              047300         PERFORM PASS GO TO ENABL-STATUS-WRITE-02.                CM1024.2
              047400     MOVE 300 TO CORRECT-2SLASH1.                                 CM1024.2
              047500     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
              047600     MOVE "/" TO SLASH.                                           CM1024.2
              047700     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
              047800     PERFORM FAIL.                                                CM1024.2
              047900     GO TO ENABL-STATUS-WRITE-02.                                 CM1024.2
              048500 ENABL-STATUS-TEST-03.                                            CM1024.2
              048600     MOVE "INVALID PASSWORD USED" TO RE-MARK.                     CM1024.2
              048700     MOVE                                                         CM1024.2
              048800     XXXXX032                                                     CM1024.2
              048900         TO SYM-DEST.                                             CM1024.2
              049000     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
              049100     MOVE 1 TO ONE.                                               CM1024.2
              049200     ENABLE OUTPUT ~~>CM-OUTQUE-1 WITH KEY                           CM1024.2
              049300         "GARBAGE".                                               CM1024.2
              049400     IF STATUS-KEY IS EQUAL TO "40"                               CM1024.2
              049500         PERFORM PASS GO TO ENABL-STATUS-WRITE-03.                CM1024.2
              049600     MOVE 400 TO CORRECT-2SLASH1.                                 CM1024.2
              049700     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
              049800     MOVE "/" TO SLASH.                                           CM1024.2
              049900     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
              050000     PERFORM FAIL.                                                CM1024.2
              050100     GO TO ENABL-STATUS-WRITE-03.                                 CM1024.2
              050700 ENABL-STATUS-TEST-04.                                            CM1024.2
              050800     MOVE "VALID ENABLE/NO ERROR EXPECTED" TO RE-MARK.            CM1024.2
              050900     MOVE                                                         CM1024.2
              051000     XXXXX032                                                     CM1024.2
              051100         TO SYM-DEST.                                             CM1024.2
              051200     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
              051300     MOVE 1 TO ONE.                                               CM1024.2
              051400     ENABLE OUTPUT ~~>CM-OUTQUE-1 WITH KEY                           CM1024.2
              051500     XXXXX033.                                                    CM1024.2
              051600     IF STATUS-KEY IS EQUAL TO ZERO                               CM1024.2
              051700         PERFORM PASS GO TO ENABL-STATUS-WRITE-04.                CM1024.2
              051800     MOVE 0 TO CORRECT-2SLASH1.                                   CM1024.2
              051900     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
              052000     MOVE "/" TO SLASH.                                           CM1024.2
              052100     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
              052200     PERFORM FAIL.                                                CM1024.2
              052300     GO TO ENABL-STATUS-WRITE-04.                                 CM1024.2
              052900 SEND-STATUS-TEST-03.                                             CM1024.2
              053000     MOVE "DESTINATION UNKNOWN" TO RE-MARK.                       CM1024.2
              053100     MOVE "GARBAGE" TO SYM-DEST.                                  CM1024.2
              053200     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
              053300     MOVE 1 TO ONE.                                               CM1024.2
              053400     MOVE 37 TO MSG-LENGTH.                                       CM1024.2
              053500     MOVE "S-03" TO TEST-IND.                                     CM1024.2
              053600     SEND ~~>CM-OUTQUE-1 FROM ERR-MSG WITH EMI.                      CM1024.2
              053700     IF STATUS-KEY IS EQUAL TO "20"                               CM1024.2
              053800         AND ERR-KEY IS EQUAL TO "1"                              CM1024.2
              053900         PERFORM PASS GO TO SEND-STATUS-WRITE-03.                 CM1024.2
              054000     MOVE 201 TO CORRECT-2SLASH1.                                 CM1024.2
              054100     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
              054200     MOVE "/" TO SLASH.                                           CM1024.2
              054300     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
              054400     PERFORM FAIL.                                                CM1024.2
              054500     GO TO SEND-STATUS-WRITE-03.                                  CM1024.2
              055100 SEND-STATUS-TEST-04.                                             CM1024.2
              055200     MOVE "DESTINATION COUNT INVALID (0)" TO RE-MARK.             CM1024.2
              055300     MOVE                                                         CM1024.2
              055400     XXXXX032                                                     CM1024.2
              055500         TO SYM-DEST.                                             CM1024.2
              055600     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
              055700     MOVE 0 TO ONE.                                               CM1024.2
              055800     MOVE 37 TO MSG-LENGTH.                                       CM1024.2
              055900     MOVE "S-04" TO TEST-IND.                                     CM1024.2
              056000     SEND ~~>CM-OUTQUE-1 FROM ERR-MSG WITH EMI.                      CM1024.2
              056100     IF STATUS-KEY IS EQUAL TO "30"                               CM1024.2
              056200         PERFORM PASS GO TO SEND-STATUS-WRITE-04.                 CM1024.2
              056300     MOVE 300 TO CORRECT-2SLASH1.                                 CM1024.2
              056400     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
              056500     MOVE "/" TO SLASH.                                           CM1024.2
              056600     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
              056700     PERFORM FAIL.                                                CM1024.2
              056800     GO TO SEND-STATUS-WRITE-04.                                  CM1024.2
              057400 SEND-STATUS-TEST-05.                                             CM1024.2
              057500     MOVE "CHARACTER COUNT EXCESSIVE" TO RE-MARK.                 CM1024.2
              057600     MOVE                                                         CM1024.2
              057700     XXXXX032                                                     CM1024.2
              057800         TO SYM-DEST.                                             CM1024.2
              057900     MOVE "9" TO STATUS-KEY ERR-KEY.                              CM1024.2
              058000     MOVE 1 TO ONE.                                               CM1024.2
              058100     MOVE 38 TO MSG-LENGTH.                                       CM1024.2
              058200     MOVE "S-05" TO TEST-IND.                                     CM1024.2
              058300     SEND ~~>CM-OUTQUE-1 FROM ERR-MSG WITH EMI.                      CM1024.2
              058400     IF STATUS-KEY IS EQUAL TO "50"                               CM1024.2
              058500         PERFORM PASS GO TO SEND-STATUS-WRITE-05.                 CM1024.2
              058600     MOVE 500 TO CORRECT-2SLASH1.                                 CM1024.2
              058700     MOVE STATUS-KEY TO COMPUTED-STATUS.                          CM1024.2
              058800     MOVE "/" TO SLASH.                                           CM1024.2
              058900     MOVE ERR-KEY TO COMPUTED-ERR-KEY.                            CM1024.2
              059000     PERFORM FAIL.                                                CM1024.2
              059100     GO TO SEND-STATUS-WRITE-05.                                  CM1024.2
              082200 SEND-EMI-A1.                                                     CM1024.2
              082300     SEND ~~>CM-OUTQUE-1 FROM MSG-OUT WITH EMI.                      CM1024.2
              082400     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              082500 SEND-EGI-A1.                                                     CM1024.2
              082600     SEND ~~>CM-OUTQUE-1 FROM MSG-OUT WITH EGI.                      CM1024.2
              082700     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              082800 SEND-EMI-AP.                                                     CM1024.2
              082900     SEND ~~>CM-OUTQUE-1 FROM MSG-OUT WITH EMI AFTER PAGE.           CM1024.2
              083000     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              083100 SEND-EMI-A3-01.                                                  CM1024.2
              083200     SEND ~~>CM-OUTQUE-1 FROM MSG-OUT WITH EMI AFTER ADVANCING 3     CM1024.2
              083300         LINES.                                                   CM1024.2
              083400     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              083500 SEND-EMI-A3-02.                                                  CM1024.2
              083600     SEND ~~>CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
              083700         AFTER ADVANCING THREE LINES.                             CM1024.2
              083800     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              083900 SEND-EMI-A3-03.                                                  CM1024.2
              084000     SEND ~~>CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
              084100         AFTER 3 LINE.                                            CM1024.2
              084200     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              084300 SEND-EMI-A3-04.                                                  CM1024.2
              084400     SEND ~~>CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
              084500         AFTER COMP-THREE.                                        CM1024.2
              084600     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              084700 SEND-EMI-A3-05.                                                  CM1024.2
              084800     SEND ~~>CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
              084900         AFTER 3.                                                 CM1024.2
              085000     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              085100 SEND-EGI-ONLY.                                                   CM1024.2
              085200     SEND ~~>CM-OUTQUE-1 WITH EGI.                                   CM1024.2
              085300     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              085400 SEND-EMI-BP.                                                     CM1024.2
              085500     SEND ~~>CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
              085600         BEFORE ADVANCING PAGE.                                   CM1024.2
              085700     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              085800 SEND-EMI-B2-01.                                                  CM1024.2
              085900     SEND ~~>CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
              086000         BEFORE ADVANCING 2 LINES.                                CM1024.2
              086100     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              086200 SEND-EMI-B2-02.                                                  CM1024.2
              086300     SEND ~~>CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
              086400         BEFORE ADVANCING TWO LINES.                              CM1024.2
              086500     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              086600 SEND-EMI-B2-03.                                                  CM1024.2
              086700     SEND ~~>CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
              086800         BEFORE 2 LINE.                                           CM1024.2
              086900     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              087000 SEND-EMI-B2-04.                                                  CM1024.2
              087100     SEND ~~>CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
              087200         BEFORE COMP-TWO.                                         CM1024.2
              087300     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              087400 SEND-EMI-B2-05.                                                  CM1024.2
              087500     SEND ~~>CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
              087600         BEFORE 2.                                                CM1024.2
              087700     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              087800 SEND-EMI-A0.                                                     CM1024.2
              087900     SEND ~~>CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
              088000         AFTER 0 LINES.                                           CM1024.2
              088100     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              088200 SEND-EMI-B0.                                                     CM1024.2
              088300     SEND ~~>CM-OUTQUE-1 FROM MSG-OUT WITH EMI                       CM1024.2
              088400         BEFORE ZERO LINES.                                       CM1024.2
              088500     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              088600 SEND-LONG-MSG.                                                   CM1024.2
              088700     SEND ~~>CM-OUTQUE-1 FROM LONG-MSG WITH EMI AFTER PAGE.          CM1024.2
              088800     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              088900 DISABLE-OUTQUE.                                                  CM1024.2
              089000     DISABLE OUTPUT ~~>CM-OUTQUE-1 KEY                               CM1024.2
              089100     PASSWORD1.                                                   CM1024.2
              089200     GO TO UNIFORM-SEND-EXIT.                                     CM1024.2
              089300 ENABLE-OUTQUE.                                                   CM1024.2
              089400     ENABLE OUTPUT ~~>CM-OUTQUE-1 WITH KEY                           CM1024.2
              089500     XXXXX033.                                                    CM1024.2
              """,
            spec -> spec.afterRecipe(cu -> {
                List<SearchResult> searchResults = new ArrayList<>(39);
                visitor.visit(cu, searchResults);
                Assertions.assertThat(searchResults).hasSize(39);
            }), true
          )
        );
    }
}
