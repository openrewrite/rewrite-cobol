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
import org.openrewrite.cobol.search.FindCopyBookReferences;
import org.openrewrite.marker.Marker;
import org.openrewrite.marker.SearchResult;
import org.openrewrite.test.RecipeSpec;

import java.util.ArrayList;
import java.util.List;

import static org.openrewrite.cobol.Assertions.cobol;

public class FindCopyBookReferencesTest extends CobolTest {

    @Override
    public void defaults(RecipeSpec spec) {
        spec.recipe(new FindCopyBookReferences("KP008"));
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
    void bookIsNotUsed() {
        rewriteRun(
          cobol(getNistResource("CM101M.CBL"), true)
        );
    }

    @Test
    void sm103A() {
        rewriteRun(
          spec -> spec.recipe(new FindCopyBookReferences(null)),
          cobol(
            """
              000100 IDENTIFICATION DIVISION.                                         SM1034.2
              000200 PROGRAM-ID.                                                      SM1034.2
              000300     SM103A.                                                      SM1034.2
              003200 SECURITY.                                                        SM1034.2
              003300     COPY K3SNA.                                                  SM1034.2
              003400 ENVIRONMENT DIVISION.                                            SM1034.2
              003500 CONFIGURATION SECTION.                                           SM1034.2
              004700 SOURCE-COMPUTER.                                      COPY K3SCA.SM1034.2
              006000 OBJECT-COMPUTER.                                      COPY K3OCA.SM1034.2
              007300 SPECIAL-NAMES.                                        COPY K3SNA.SM1034.2
              007500 INPUT-OUTPUT SECTION.                                            SM1034.2
              008700 FILE-CONTROL.                                         COPY K3FCA.SM1034.2
              010000 I-O-CONTROL.                                          COPY K3IOA.SM1034.2
              029400 PROCEDURE DIVISION.                                              SM1034.2
              029500 CCVS1 SECTION.                                                   SM1034.2
              029600 OPEN-FILES.                                                      SM1034.2
              029700     OPEN     OUTPUT PRINT-FILE.                                  SM1034.2
              029800     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   SM1034.2
              029900     MOVE    SPACE TO TEST-RESULTS.                               SM1034.2
              030000     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             SM1034.2
              030100     GO TO CCVS1-EXIT.                                            SM1034.2
              030200 CLOSE-FILES.                                                     SM1034.2
              030300     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   SM1034.2
              030400 TERMINATE-CCVS.                                                  SM1034.2
              030500S    EXIT PROGRAM.                                                SM1034.2
              030600STERMINATE-CALL.                                                  SM1034.2
              030700     STOP     RUN.                                                SM1034.2
              030800 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         SM1034.2
              030900 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           SM1034.2
              031000 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          SM1034.2
              031100 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      SM1034.2
              031200     MOVE "****TEST DELETED****" TO RE-MARK.                      SM1034.2
              031300 PRINT-DETAIL.                                                    SM1034.2
              031400     IF REC-CT NOT EQUAL TO ZERO                                  SM1034.2
              031500             MOVE "." TO PARDOT-X                                 SM1034.2
              031600             MOVE REC-CT TO DOTVALUE.                             SM1034.2
              031700     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      SM1034.2
              031800     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               SM1034.2
              031900        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 SM1034.2
              032000          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 SM1034.2
              032100     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              SM1034.2
              032200     MOVE SPACE TO CORRECT-X.                                     SM1034.2
              032300     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         SM1034.2
              032400     MOVE     SPACE TO RE-MARK.                                   SM1034.2
              032500 HEAD-ROUTINE.                                                    SM1034.2
              032600     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  SM1034.2
              032700     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  SM1034.2
              032800     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  SM1034.2
              032900     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  SM1034.2
              033000 COLUMN-NAMES-ROUTINE.                                            SM1034.2
              033100     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM1034.2
              033200     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM1034.2
              033300     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        SM1034.2
              033400 END-ROUTINE.                                                     SM1034.2
              033500     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.SM1034.2
              033600 END-RTN-EXIT.                                                    SM1034.2
              033700     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM1034.2
              033800 END-ROUTINE-1.                                                   SM1034.2
              033900      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      SM1034.2
              034000      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               SM1034.2
              034100      ADD PASS-COUNTER TO ERROR-HOLD.                             SM1034.2
              034200*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   SM1034.2
              034300      MOVE PASS-COUNTER TO CCVS-E-4-1.                            SM1034.2
              034400      MOVE ERROR-HOLD TO CCVS-E-4-2.                              SM1034.2
              034500      MOVE CCVS-E-4 TO CCVS-E-2-2.                                SM1034.2
              034600      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           SM1034.2
              034700  END-ROUTINE-12.                                                 SM1034.2
              034800      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        SM1034.2
              034900     IF       ERROR-COUNTER IS EQUAL TO ZERO                      SM1034.2
              035000         MOVE "NO " TO ERROR-TOTAL                                SM1034.2
              035100         ELSE                                                     SM1034.2
              035200         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       SM1034.2
              035300     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           SM1034.2
              035400     PERFORM WRITE-LINE.                                          SM1034.2
              035500 END-ROUTINE-13.                                                  SM1034.2
              035600     IF DELETE-COUNTER IS EQUAL TO ZERO                           SM1034.2
              035700         MOVE "NO " TO ERROR-TOTAL  ELSE                          SM1034.2
              035800         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      SM1034.2
              035900     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   SM1034.2
              036000     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM1034.2
              036100      IF   INSPECT-COUNTER EQUAL TO ZERO                          SM1034.2
              036200          MOVE "NO " TO ERROR-TOTAL                               SM1034.2
              036300      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   SM1034.2
              036400      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            SM1034.2
              036500      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          SM1034.2
              036600     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM1034.2
              036700 WRITE-LINE.                                                      SM1034.2
              036800     ADD 1 TO RECORD-COUNT.                                       SM1034.2
              036900Y    IF RECORD-COUNT GREATER 50                                   SM1034.2
              037000Y        MOVE DUMMY-RECORD TO DUMMY-HOLD                          SM1034.2
              037100Y        MOVE SPACE TO DUMMY-RECORD                               SM1034.2
              037200Y        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  SM1034.2
              037300Y        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             SM1034.2
              037400Y        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     SM1034.2
              037500Y        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          SM1034.2
              037600Y        MOVE DUMMY-HOLD TO DUMMY-RECORD                          SM1034.2
              037700Y        MOVE ZERO TO RECORD-COUNT.                               SM1034.2
              037800     PERFORM WRT-LN.                                              SM1034.2
              037900 WRT-LN.                                                          SM1034.2
              038000     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               SM1034.2
              038100     MOVE SPACE TO DUMMY-RECORD.                                  SM1034.2
              038200 BLANK-LINE-PRINT.                                                SM1034.2
              038300     PERFORM WRT-LN.                                              SM1034.2
              038400 FAIL-ROUTINE.                                                    SM1034.2
              038500     IF   COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE. SM1034.2
              038600     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.SM1034.2
              038700     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 SM1034.2
              038800     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   SM1034.2
              038900     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM1034.2
              039000     MOVE   SPACES TO INF-ANSI-REFERENCE.                         SM1034.2
              039100     GO TO  FAIL-ROUTINE-EX.                                      SM1034.2
              039200 FAIL-ROUTINE-WRITE.                                              SM1034.2
              039300     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         SM1034.2
              039400     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 SM1034.2
              039500     MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. SM1034.2
              039600     MOVE   SPACES TO COR-ANSI-REFERENCE.                         SM1034.2
              039700 FAIL-ROUTINE-EX. EXIT.                                           SM1034.2
              039800 BAIL-OUT.                                                        SM1034.2
              039900     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   SM1034.2
              040000     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           SM1034.2
              040100 BAIL-OUT-WRITE.                                                  SM1034.2
              040200     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  SM1034.2
              040300     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 SM1034.2
              040400     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM1034.2
              040500     MOVE   SPACES TO INF-ANSI-REFERENCE.                         SM1034.2
              040600 BAIL-OUT-EX. EXIT.                                               SM1034.2
              040700 CCVS1-EXIT.                                                      SM1034.2
              040800     EXIT.                                                        SM1034.2
              040900 INITIALIZATION SECTION.                                          SM1034.2
              041000 SM103A-INIT.                                                     SM1034.2
              041100     MOVE     "ALL TESTS IN SM103A PRODUCE" TO RE-MARK.           SM1034.2
              041200     PERFORM  PRINT-DETAIL.                                       SM1034.2
              041300     MOVE     "OUTPUT CHECKED IN SM104A."   TO RE-MARK.           SM1034.2
              041400     PERFORM  PRINT-DETAIL.                                       SM1034.2
              041500     MOVE     "COPY ---" TO FEATURE.                              SM1034.2
              041600     PERFORM  PRINT-DETAIL.                                       SM1034.2
              041700 SPECIAL-NAMES-TEST SECTION.                                      SM1034.2
              041800 COPY-TEST-1.                                                     SM1034.2
              041900     MOVE     S-N-1 TO S-N-2.                                     SM1034.2
              042000*    NOTE     THIS ROUTINE USES A COPIED DECIMAL-POINT IS COMMA   SM1034.2
              042100*             CLAUSE IN SPECIAL-NAMES --- THE EDITING IN S-N-2    SM1034.2
              042200*             WOULD NOT BE VALID WITHOUT THIS CLAUSE.             SM1034.2
              042300     PERFORM  PASS.                                               SM1034.2
              042400     GO       TO COPY-WRITE-1.                                    SM1034.2
              042500 COPY-DELETE-1.                                                   SM1034.2
              042600     PERFORM  DE-LETE.                                            SM1034.2
              042700 COPY-WRITE-1.                                                    SM1034.2
              042800     MOVE     "  DEC POINT IS COMMA" TO FEATURE.                  SM1034.2
              042900     MOVE     "COPY-TEST-1 " TO PAR-NAME.                         SM1034.2
              043000     PERFORM  PRINT-DETAIL.                                       SM1034.2
              043100 BUILD SECTION.                                                   SM1034.2
              043200 COPY-TEST-2.                                                     SM1034.2
              043300     OPEN     OUTPUT TEST-FILE.                                   SM1034.2
              043400     MOVE     S-N-2 TO TST-FLD-2.                                 SM1034.2
              043500     MOVE     RCD-1 TO TST-FLD-1.                                 SM1034.2
              043600     WRITE    TEST-REC.                                           SM1034.2
              043700     MOVE     RCD-2 TO TST-FLD-1.                                 SM1034.2
              043800     WRITE    TEST-REC.                                           SM1034.2
              043900     MOVE     RCD-3 TO TST-FLD-1.                                 SM1034.2
              044000     WRITE    TEST-REC.                                           SM1034.2
              044100     MOVE     RCD-4 TO TST-FLD-1.                                 SM1034.2
              044200     WRITE    TEST-REC.                                           SM1034.2
              044300     MOVE     RCD-5 TO TST-FLD-1.                                 SM1034.2
              044400     WRITE    TEST-REC.                                           SM1034.2
              044500     MOVE     RCD-6 TO TST-FLD-1.                                 SM1034.2
              044600     WRITE    TEST-REC.                                           SM1034.2
              044700     MOVE     RCD-7 TO TST-FLD-1.                                 SM1034.2
              044800     WRITE    TEST-REC.                                           SM1034.2
              044900     CLOSE    TEST-FILE.                                          SM1034.2
              045000     OPEN OUTPUT TEST-FILE2.                                      SM1034.2
              045100     MOVE ZERO TO TST-FLD-3.                                      SM1034.2
              045200     MOVE "DDDDD" TO TST-FLD-4.                                   SM1034.2
              045300     WRITE TEST-REC2.                                             SM1034.2
              045400     CLOSE TEST-FILE2.                                            SM1034.2
              045500     PERFORM  PASS.                                               SM1034.2
              045600     GO       TO COPY-WRITE-2.                                    SM1034.2
              045700 COPY-DELETE-2.                                                   SM1034.2
              045800     PERFORM DE-LETE.                                             SM1034.2
              045900 COPY-WRITE-2.                                                    SM1034.2
              046000     MOVE   "  ENVIR DIV ENTRIES" TO FEATURE.                     SM1034.2
              046100     MOVE   "COPY-TEST-2 " TO PAR-NAME.                           SM1034.2
              046200     PERFORM PRINT-DETAIL.                                        SM1034.2
              046300*                                                                 SM1034.2
              046400 COPY-TEST-3.                                                     SM1034.2
              046500*    ===-->  MINIMUM LENGTH TEXT WORD  <--===                     SM1034.2
              046600     MOVE   "XII-2 2.3 SR8" TO ANSI-REFERENCE.                    SM1034.2
              046700     MOVE   "COPY-TEST-3"   TO PAR-NAME.                          SM1034.2
              046800     MOVE    8  TO WRK-DU-00001.                                  SM1034.2
              046900     GO TO   COPY-TEST-3-0.                                       SM1034.2
              047000 COPY-DELETE-3.                                                   SM1034.2
              047100     PERFORM DE-LETE.                                             SM1034.2
              047200     PERFORM PRINT-DETAIL.                                        SM1034.2
              047300     GO TO   COPY-INIT-4.                                         SM1034.2
              047400 COPY-TEST-3-0.                                                   SM1034.2
              047800     IF      WRK-DU-00001 =                                       SM1034.2
              047900     COPY    K3SML.                                               SM1034.2
              048000             PERFORM PASS                                         SM1034.2
              048100             PERFORM PRINT-DETAIL                                 SM1034.2
              048200     ELSE                                                         SM1034.2
              048300             MOVE   "COPYING SINGLE CHARACTER FAILED"             SM1034.2
              048400                  TO RE-MARK                                      SM1034.2
              048500             MOVE    8   TO CORRECT-N                             SM1034.2
              048600             MOVE    WRK-DU-00001 TO COMPUTED-N                   SM1034.2
              048700             PERFORM FAIL                                         SM1034.2
              048800             PERFORM PRINT-DETAIL.                                SM1034.2
              049000 COPY-INIT-4.                                                     SM1034.2
              049200     MOVE   "XII-2 2.3 (SR8) AND XII-5 2.4(GR11)"                 SM1034.2
              049300          TO ANSI-REFERENCE.                                      SM1034.2
              049400     MOVE   "COPY-TEST-4" TO PAR-NAME.                            SM1034.2
              049500     MOVE    SPACES      TO WRK-XN-00322.                         SM1034.2
              049600     MOVE    1 TO REC-CT.                                         SM1034.2
              049700     GO TO   COPY-TEST-4-0.                                       SM1034.2
              049800 COPY-DELETE-4.                                                   SM1034.2
              049900     PERFORM DE-LETE.                                             SM1034.2
              050000     PERFORM PRINT-DETAIL.                                        SM1034.2
              050100     GO TO   CCVS-EXIT.                                           SM1034.2
              050200 COPY-TEST-4-0.                                                   SM1034.2
              051200     COPY    K3LGE.                                               SM1034.2
              051400 COPY-TEST-4-1.                                                   SM1034.2
              051500     MOVE   "COPY-TEST-4-1" TO PAR-NAME.                          SM1034.2
              051600     IF      WRK-DU-9 = 6                                         SM1034.2
              051700             PERFORM PASS                                         SM1034.2
              051800             PERFORM PRINT-DETAIL                                 SM1034.2
              051900     ELSE                                                         SM1034.2
              052000             MOVE   "COPYING ALL 322 CHARACTERS FAILED"           SM1034.2
              052100                  TO RE-MARK                                      SM1034.2
              052200             MOVE    6   TO CORRECT-N                             SM1034.2
              052300             MOVE    WRK-DU-9 TO COMPUTED-N                       SM1034.2
              052400             PERFORM FAIL                                         SM1034.2
              052500             PERFORM PRINT-DETAIL.                                SM1034.2
              052600     ADD     1 TO REC-CT.                                         SM1034.2
              052700 COPY-TEST-4-2.                                                   SM1034.2
              052800     MOVE   "COPY-TEST-4-2" TO PAR-NAME.                          SM1034.2
              052900     IF      WRK-DU-99 = 9                                        SM1034.2
              053000             PERFORM PASS                                         SM1034.2
              053100             PERFORM PRINT-DETAIL                                 SM1034.2
              053200     ELSE                                                         SM1034.2
              053300             MOVE   "COPYING ALL 322 CHARACTERS FAILED"           SM1034.2
              053400                  TO RE-MARK                                      SM1034.2
              053500             MOVE    9   TO CORRECT-N                             SM1034.2
              053600             MOVE    WRK-DU-99 TO COMPUTED-N                      SM1034.2
              053700             PERFORM FAIL                                         SM1034.2
              053800             PERFORM PRINT-DETAIL.                                SM1034.2
              053900     ADD     1 TO REC-CT.                                         SM1034.2
              054000 COPY-TEST-4-3.                                                   SM1034.2
              054100     MOVE   "COPY-TEST-4-3" TO PAR-NAME.                          SM1034.2
              054200     IF      WRK-DU-99-LONGER = 10                                SM1034.2
              054300             PERFORM PASS                                         SM1034.2
              054400             PERFORM PRINT-DETAIL                                 SM1034.2
              054500     ELSE                                                         SM1034.2
              054600             MOVE   "COPYING ALL 322 CHARACTERS FAILED"           SM1034.2
              054700                  TO RE-MARK                                      SM1034.2
              054800             MOVE    10  TO CORRECT-N                             SM1034.2
              054900             MOVE    WRK-DU-99-LONGER TO COMPUTED-N               SM1034.2
              055000             PERFORM FAIL                                         SM1034.2
              055100             PERFORM PRINT-DETAIL.                                SM1034.2
              055300 CCVS-EXIT SECTION.                                               SM1034.2
              055400 CCVS-999999.                                                     SM1034.2
              055500     GO TO CLOSE-FILES.                                           SM1034.2
              """,
            """
              000100 IDENTIFICATION DIVISION.                                         SM1034.2
              000200 PROGRAM-ID.                                                      SM1034.2
              000300     SM103A.                                                      SM1034.2
              003200 SECURITY.                                                        SM1034.2
              003300     COPY K3SNA.                                                  SM1034.2
              003400 ENVIRONMENT DIVISION.                                            SM1034.2
              003500 CONFIGURATION SECTION.                                           SM1034.2
              004700 SOURCE-COMPUTER.                                      COPY ~~>K3SCA.SM1034.2
              006000 OBJECT-COMPUTER.                                      COPY ~~>K3OCA.SM1034.2
              007300 SPECIAL-NAMES.                                        COPY ~~>K3SNA.SM1034.2
              007500 INPUT-OUTPUT SECTION.                                            SM1034.2
              008700 FILE-CONTROL.                                         COPY ~~>K3FCA.SM1034.2
              010000 I-O-CONTROL.                                          COPY ~~>K3IOA.SM1034.2
              029400 PROCEDURE DIVISION.                                              SM1034.2
              029500 CCVS1 SECTION.                                                   SM1034.2
              029600 OPEN-FILES.                                                      SM1034.2
              029700     OPEN     OUTPUT PRINT-FILE.                                  SM1034.2
              029800     MOVE CCVS-PGM-ID TO TEST-ID. MOVE CCVS-PGM-ID TO ID-AGAIN.   SM1034.2
              029900     MOVE    SPACE TO TEST-RESULTS.                               SM1034.2
              030000     PERFORM  HEAD-ROUTINE THRU COLUMN-NAMES-ROUTINE.             SM1034.2
              030100     GO TO CCVS1-EXIT.                                            SM1034.2
              030200 CLOSE-FILES.                                                     SM1034.2
              030300     PERFORM END-ROUTINE THRU END-ROUTINE-13. CLOSE PRINT-FILE.   SM1034.2
              030400 TERMINATE-CCVS.                                                  SM1034.2
              030500S    EXIT PROGRAM.                                                SM1034.2
              030600STERMINATE-CALL.                                                  SM1034.2
              030700     STOP     RUN.                                                SM1034.2
              030800 INSPT. MOVE "INSPT" TO P-OR-F. ADD 1 TO INSPECT-COUNTER.         SM1034.2
              030900 PASS.  MOVE "PASS " TO P-OR-F.  ADD 1 TO PASS-COUNTER.           SM1034.2
              031000 FAIL.  MOVE "FAIL*" TO P-OR-F.  ADD 1 TO ERROR-COUNTER.          SM1034.2
              031100 DE-LETE.  MOVE "*****" TO P-OR-F.  ADD 1 TO DELETE-COUNTER.      SM1034.2
              031200     MOVE "****TEST DELETED****" TO RE-MARK.                      SM1034.2
              031300 PRINT-DETAIL.                                                    SM1034.2
              031400     IF REC-CT NOT EQUAL TO ZERO                                  SM1034.2
              031500             MOVE "." TO PARDOT-X                                 SM1034.2
              031600             MOVE REC-CT TO DOTVALUE.                             SM1034.2
              031700     MOVE     TEST-RESULTS TO PRINT-REC. PERFORM WRITE-LINE.      SM1034.2
              031800     IF P-OR-F EQUAL TO "FAIL*"  PERFORM WRITE-LINE               SM1034.2
              031900        PERFORM FAIL-ROUTINE THRU FAIL-ROUTINE-EX                 SM1034.2
              032000          ELSE PERFORM BAIL-OUT THRU BAIL-OUT-EX.                 SM1034.2
              032100     MOVE SPACE TO P-OR-F. MOVE SPACE TO COMPUTED-X.              SM1034.2
              032200     MOVE SPACE TO CORRECT-X.                                     SM1034.2
              032300     IF     REC-CT EQUAL TO ZERO  MOVE SPACE TO PAR-NAME.         SM1034.2
              032400     MOVE     SPACE TO RE-MARK.                                   SM1034.2
              032500 HEAD-ROUTINE.                                                    SM1034.2
              032600     MOVE CCVS-H-1  TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  SM1034.2
              032700     MOVE CCVS-H-2A TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.  SM1034.2
              032800     MOVE CCVS-H-2B TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  SM1034.2
              032900     MOVE CCVS-H-3  TO DUMMY-RECORD. PERFORM WRITE-LINE 3 TIMES.  SM1034.2
              033000 COLUMN-NAMES-ROUTINE.                                            SM1034.2
              033100     MOVE CCVS-C-1 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM1034.2
              033200     MOVE CCVS-C-2 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM1034.2
              033300     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE.        SM1034.2
              033400 END-ROUTINE.                                                     SM1034.2
              033500     MOVE HYPHEN-LINE TO DUMMY-RECORD. PERFORM WRITE-LINE 5 TIMES.SM1034.2
              033600 END-RTN-EXIT.                                                    SM1034.2
              033700     MOVE CCVS-E-1 TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM1034.2
              033800 END-ROUTINE-1.                                                   SM1034.2
              033900      ADD ERROR-COUNTER TO ERROR-HOLD ADD INSPECT-COUNTER TO      SM1034.2
              034000      ERROR-HOLD. ADD DELETE-COUNTER TO ERROR-HOLD.               SM1034.2
              034100      ADD PASS-COUNTER TO ERROR-HOLD.                             SM1034.2
              034200*     IF PASS-COUNTER EQUAL TO ERROR-HOLD GO TO END-ROUTINE-12.   SM1034.2
              034300      MOVE PASS-COUNTER TO CCVS-E-4-1.                            SM1034.2
              034400      MOVE ERROR-HOLD TO CCVS-E-4-2.                              SM1034.2
              034500      MOVE CCVS-E-4 TO CCVS-E-2-2.                                SM1034.2
              034600      MOVE CCVS-E-2 TO DUMMY-RECORD PERFORM WRITE-LINE.           SM1034.2
              034700  END-ROUTINE-12.                                                 SM1034.2
              034800      MOVE "TEST(S) FAILED" TO ENDER-DESC.                        SM1034.2
              034900     IF       ERROR-COUNTER IS EQUAL TO ZERO                      SM1034.2
              035000         MOVE "NO " TO ERROR-TOTAL                                SM1034.2
              035100         ELSE                                                     SM1034.2
              035200         MOVE ERROR-COUNTER TO ERROR-TOTAL.                       SM1034.2
              035300     MOVE     CCVS-E-2 TO DUMMY-RECORD.                           SM1034.2
              035400     PERFORM WRITE-LINE.                                          SM1034.2
              035500 END-ROUTINE-13.                                                  SM1034.2
              035600     IF DELETE-COUNTER IS EQUAL TO ZERO                           SM1034.2
              035700         MOVE "NO " TO ERROR-TOTAL  ELSE                          SM1034.2
              035800         MOVE DELETE-COUNTER TO ERROR-TOTAL.                      SM1034.2
              035900     MOVE "TEST(S) DELETED     " TO ENDER-DESC.                   SM1034.2
              036000     MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM1034.2
              036100      IF   INSPECT-COUNTER EQUAL TO ZERO                          SM1034.2
              036200          MOVE "NO " TO ERROR-TOTAL                               SM1034.2
              036300      ELSE MOVE INSPECT-COUNTER TO ERROR-TOTAL.                   SM1034.2
              036400      MOVE "TEST(S) REQUIRE INSPECTION" TO ENDER-DESC.            SM1034.2
              036500      MOVE CCVS-E-2 TO DUMMY-RECORD. PERFORM WRITE-LINE.          SM1034.2
              036600     MOVE CCVS-E-3 TO DUMMY-RECORD. PERFORM WRITE-LINE.           SM1034.2
              036700 WRITE-LINE.                                                      SM1034.2
              036800     ADD 1 TO RECORD-COUNT.                                       SM1034.2
              036900Y    IF RECORD-COUNT GREATER 50                                   SM1034.2
              037000Y        MOVE DUMMY-RECORD TO DUMMY-HOLD                          SM1034.2
              037100Y        MOVE SPACE TO DUMMY-RECORD                               SM1034.2
              037200Y        WRITE DUMMY-RECORD AFTER ADVANCING PAGE                  SM1034.2
              037300Y        MOVE CCVS-C-1 TO DUMMY-RECORD PERFORM WRT-LN             SM1034.2
              037400Y        MOVE CCVS-C-2 TO DUMMY-RECORD PERFORM WRT-LN 2 TIMES     SM1034.2
              037500Y        MOVE HYPHEN-LINE TO DUMMY-RECORD PERFORM WRT-LN          SM1034.2
              037600Y        MOVE DUMMY-HOLD TO DUMMY-RECORD                          SM1034.2
              037700Y        MOVE ZERO TO RECORD-COUNT.                               SM1034.2
              037800     PERFORM WRT-LN.                                              SM1034.2
              037900 WRT-LN.                                                          SM1034.2
              038000     WRITE    DUMMY-RECORD AFTER ADVANCING 1 LINES.               SM1034.2
              038100     MOVE SPACE TO DUMMY-RECORD.                                  SM1034.2
              038200 BLANK-LINE-PRINT.                                                SM1034.2
              038300     PERFORM WRT-LN.                                              SM1034.2
              038400 FAIL-ROUTINE.                                                    SM1034.2
              038500     IF   COMPUTED-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE. SM1034.2
              038600     IF     CORRECT-X NOT EQUAL TO SPACE GO TO FAIL-ROUTINE-WRITE.SM1034.2
              038700     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 SM1034.2
              038800     MOVE  "NO FURTHER INFORMATION, SEE PROGRAM." TO INFO-TEXT.   SM1034.2
              038900     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM1034.2
              039000     MOVE   SPACES TO INF-ANSI-REFERENCE.                         SM1034.2
              039100     GO TO  FAIL-ROUTINE-EX.                                      SM1034.2
              039200 FAIL-ROUTINE-WRITE.                                              SM1034.2
              039300     MOVE   TEST-COMPUTED TO PRINT-REC PERFORM WRITE-LINE         SM1034.2
              039400     MOVE   ANSI-REFERENCE TO COR-ANSI-REFERENCE.                 SM1034.2
              039500     MOVE   TEST-CORRECT TO PRINT-REC PERFORM WRITE-LINE 2 TIMES. SM1034.2
              039600     MOVE   SPACES TO COR-ANSI-REFERENCE.                         SM1034.2
              039700 FAIL-ROUTINE-EX. EXIT.                                           SM1034.2
              039800 BAIL-OUT.                                                        SM1034.2
              039900     IF     COMPUTED-A NOT EQUAL TO SPACE GO TO BAIL-OUT-WRITE.   SM1034.2
              040000     IF     CORRECT-A EQUAL TO SPACE GO TO BAIL-OUT-EX.           SM1034.2
              040100 BAIL-OUT-WRITE.                                                  SM1034.2
              040200     MOVE CORRECT-A TO XXCORRECT. MOVE COMPUTED-A TO XXCOMPUTED.  SM1034.2
              040300     MOVE   ANSI-REFERENCE TO INF-ANSI-REFERENCE.                 SM1034.2
              040400     MOVE   XXINFO TO DUMMY-RECORD. PERFORM WRITE-LINE 2 TIMES.   SM1034.2
              040500     MOVE   SPACES TO INF-ANSI-REFERENCE.                         SM1034.2
              040600 BAIL-OUT-EX. EXIT.                                               SM1034.2
              040700 CCVS1-EXIT.                                                      SM1034.2
              040800     EXIT.                                                        SM1034.2
              040900 INITIALIZATION SECTION.                                          SM1034.2
              041000 SM103A-INIT.                                                     SM1034.2
              041100     MOVE     "ALL TESTS IN SM103A PRODUCE" TO RE-MARK.           SM1034.2
              041200     PERFORM  PRINT-DETAIL.                                       SM1034.2
              041300     MOVE     "OUTPUT CHECKED IN SM104A."   TO RE-MARK.           SM1034.2
              041400     PERFORM  PRINT-DETAIL.                                       SM1034.2
              041500     MOVE     "COPY ---" TO FEATURE.                              SM1034.2
              041600     PERFORM  PRINT-DETAIL.                                       SM1034.2
              041700 SPECIAL-NAMES-TEST SECTION.                                      SM1034.2
              041800 COPY-TEST-1.                                                     SM1034.2
              041900     MOVE     S-N-1 TO S-N-2.                                     SM1034.2
              042000*    NOTE     THIS ROUTINE USES A COPIED DECIMAL-POINT IS COMMA   SM1034.2
              042100*             CLAUSE IN SPECIAL-NAMES --- THE EDITING IN S-N-2    SM1034.2
              042200*             WOULD NOT BE VALID WITHOUT THIS CLAUSE.             SM1034.2
              042300     PERFORM  PASS.                                               SM1034.2
              042400     GO       TO COPY-WRITE-1.                                    SM1034.2
              042500 COPY-DELETE-1.                                                   SM1034.2
              042600     PERFORM  DE-LETE.                                            SM1034.2
              042700 COPY-WRITE-1.                                                    SM1034.2
              042800     MOVE     "  DEC POINT IS COMMA" TO FEATURE.                  SM1034.2
              042900     MOVE     "COPY-TEST-1 " TO PAR-NAME.                         SM1034.2
              043000     PERFORM  PRINT-DETAIL.                                       SM1034.2
              043100 BUILD SECTION.                                                   SM1034.2
              043200 COPY-TEST-2.                                                     SM1034.2
              043300     OPEN     OUTPUT TEST-FILE.                                   SM1034.2
              043400     MOVE     S-N-2 TO TST-FLD-2.                                 SM1034.2
              043500     MOVE     RCD-1 TO TST-FLD-1.                                 SM1034.2
              043600     WRITE    TEST-REC.                                           SM1034.2
              043700     MOVE     RCD-2 TO TST-FLD-1.                                 SM1034.2
              043800     WRITE    TEST-REC.                                           SM1034.2
              043900     MOVE     RCD-3 TO TST-FLD-1.                                 SM1034.2
              044000     WRITE    TEST-REC.                                           SM1034.2
              044100     MOVE     RCD-4 TO TST-FLD-1.                                 SM1034.2
              044200     WRITE    TEST-REC.                                           SM1034.2
              044300     MOVE     RCD-5 TO TST-FLD-1.                                 SM1034.2
              044400     WRITE    TEST-REC.                                           SM1034.2
              044500     MOVE     RCD-6 TO TST-FLD-1.                                 SM1034.2
              044600     WRITE    TEST-REC.                                           SM1034.2
              044700     MOVE     RCD-7 TO TST-FLD-1.                                 SM1034.2
              044800     WRITE    TEST-REC.                                           SM1034.2
              044900     CLOSE    TEST-FILE.                                          SM1034.2
              045000     OPEN OUTPUT TEST-FILE2.                                      SM1034.2
              045100     MOVE ZERO TO TST-FLD-3.                                      SM1034.2
              045200     MOVE "DDDDD" TO TST-FLD-4.                                   SM1034.2
              045300     WRITE TEST-REC2.                                             SM1034.2
              045400     CLOSE TEST-FILE2.                                            SM1034.2
              045500     PERFORM  PASS.                                               SM1034.2
              045600     GO       TO COPY-WRITE-2.                                    SM1034.2
              045700 COPY-DELETE-2.                                                   SM1034.2
              045800     PERFORM DE-LETE.                                             SM1034.2
              045900 COPY-WRITE-2.                                                    SM1034.2
              046000     MOVE   "  ENVIR DIV ENTRIES" TO FEATURE.                     SM1034.2
              046100     MOVE   "COPY-TEST-2 " TO PAR-NAME.                           SM1034.2
              046200     PERFORM PRINT-DETAIL.                                        SM1034.2
              046300*                                                                 SM1034.2
              046400 COPY-TEST-3.                                                     SM1034.2
              046500*    ===-->  MINIMUM LENGTH TEXT WORD  <--===                     SM1034.2
              046600     MOVE   "XII-2 2.3 SR8" TO ANSI-REFERENCE.                    SM1034.2
              046700     MOVE   "COPY-TEST-3"   TO PAR-NAME.                          SM1034.2
              046800     MOVE    8  TO WRK-DU-00001.                                  SM1034.2
              046900     GO TO   COPY-TEST-3-0.                                       SM1034.2
              047000 COPY-DELETE-3.                                                   SM1034.2
              047100     PERFORM DE-LETE.                                             SM1034.2
              047200     PERFORM PRINT-DETAIL.                                        SM1034.2
              047300     GO TO   COPY-INIT-4.                                         SM1034.2
              047400 COPY-TEST-3-0.                                                   SM1034.2
              047800     IF      WRK-DU-00001 =                                       SM1034.2
              047900     COPY    ~~>K3SML.                                               SM1034.2
              048000             PERFORM PASS                                         SM1034.2
              048100             PERFORM PRINT-DETAIL                                 SM1034.2
              048200     ELSE                                                         SM1034.2
              048300             MOVE   "COPYING SINGLE CHARACTER FAILED"             SM1034.2
              048400                  TO RE-MARK                                      SM1034.2
              048500             MOVE    8   TO CORRECT-N                             SM1034.2
              048600             MOVE    WRK-DU-00001 TO COMPUTED-N                   SM1034.2
              048700             PERFORM FAIL                                         SM1034.2
              048800             PERFORM PRINT-DETAIL.                                SM1034.2
              049000 COPY-INIT-4.                                                     SM1034.2
              049200     MOVE   "XII-2 2.3 (SR8) AND XII-5 2.4(GR11)"                 SM1034.2
              049300          TO ANSI-REFERENCE.                                      SM1034.2
              049400     MOVE   "COPY-TEST-4" TO PAR-NAME.                            SM1034.2
              049500     MOVE    SPACES      TO WRK-XN-00322.                         SM1034.2
              049600     MOVE    1 TO REC-CT.                                         SM1034.2
              049700     GO TO   COPY-TEST-4-0.                                       SM1034.2
              049800 COPY-DELETE-4.                                                   SM1034.2
              049900     PERFORM DE-LETE.                                             SM1034.2
              050000     PERFORM PRINT-DETAIL.                                        SM1034.2
              050100     GO TO   CCVS-EXIT.                                           SM1034.2
              050200 COPY-TEST-4-0.                                                   SM1034.2
              051200     COPY    ~~>K3LGE.                                               SM1034.2
              051400 COPY-TEST-4-1.                                                   SM1034.2
              051500     MOVE   "COPY-TEST-4-1" TO PAR-NAME.                          SM1034.2
              051600     IF      WRK-DU-9 = 6                                         SM1034.2
              051700             PERFORM PASS                                         SM1034.2
              051800             PERFORM PRINT-DETAIL                                 SM1034.2
              051900     ELSE                                                         SM1034.2
              052000             MOVE   "COPYING ALL 322 CHARACTERS FAILED"           SM1034.2
              052100                  TO RE-MARK                                      SM1034.2
              052200             MOVE    6   TO CORRECT-N                             SM1034.2
              052300             MOVE    WRK-DU-9 TO COMPUTED-N                       SM1034.2
              052400             PERFORM FAIL                                         SM1034.2
              052500             PERFORM PRINT-DETAIL.                                SM1034.2
              052600     ADD     1 TO REC-CT.                                         SM1034.2
              052700 COPY-TEST-4-2.                                                   SM1034.2
              052800     MOVE   "COPY-TEST-4-2" TO PAR-NAME.                          SM1034.2
              052900     IF      WRK-DU-99 = 9                                        SM1034.2
              053000             PERFORM PASS                                         SM1034.2
              053100             PERFORM PRINT-DETAIL                                 SM1034.2
              053200     ELSE                                                         SM1034.2
              053300             MOVE   "COPYING ALL 322 CHARACTERS FAILED"           SM1034.2
              053400                  TO RE-MARK                                      SM1034.2
              053500             MOVE    9   TO CORRECT-N                             SM1034.2
              053600             MOVE    WRK-DU-99 TO COMPUTED-N                      SM1034.2
              053700             PERFORM FAIL                                         SM1034.2
              053800             PERFORM PRINT-DETAIL.                                SM1034.2
              053900     ADD     1 TO REC-CT.                                         SM1034.2
              054000 COPY-TEST-4-3.                                                   SM1034.2
              054100     MOVE   "COPY-TEST-4-3" TO PAR-NAME.                          SM1034.2
              054200     IF      WRK-DU-99-LONGER = 10                                SM1034.2
              054300             PERFORM PASS                                         SM1034.2
              054400             PERFORM PRINT-DETAIL                                 SM1034.2
              054500     ELSE                                                         SM1034.2
              054600             MOVE   "COPYING ALL 322 CHARACTERS FAILED"           SM1034.2
              054700                  TO RE-MARK                                      SM1034.2
              054800             MOVE    10  TO CORRECT-N                             SM1034.2
              054900             MOVE    WRK-DU-99-LONGER TO COMPUTED-N               SM1034.2
              055000             PERFORM FAIL                                         SM1034.2
              055100             PERFORM PRINT-DETAIL.                                SM1034.2
              055300 CCVS-EXIT SECTION.                                               SM1034.2
              055400 CCVS-999999.                                                     SM1034.2
              055500     GO TO CLOSE-FILES.                                           SM1034.2
              """,
            spec -> spec.afterRecipe(cu -> {
                List<SearchResult> searchResults = new ArrayList<>(7);
                visitor.visit(cu, searchResults);
                Assertions.assertThat(searchResults).hasSize(7);
            }), true)
        );
    }

    @Test
    void sm206a() {
        rewriteRun(
          cobol(
            """
              000100 IDENTIFICATION DIVISION.                                         SM2064.2
              000200 PROGRAM-ID.                                                      SM2064.2
              000300     SM206A.                                                      SM2064.2
              021500 PROCEDURE DIVISION.                                              SM2064.2
              021600 CCVS1 SECTION.                                                   SM2064.2
              032800 CCVS1-EXIT.                                                      SM2064.2
              032900     EXIT.                                                        SM2064.2
              033000 SECT-SM206-0001 SECTION.                                         SM2064.2
              033800     COPY                                                    KP001SM2064.2
              033900             REPLACING ==PERFORM FAIL. == BY ====.                SM2064.2
              034100 SECT-SM206-0002 SECTION.                                         SM2064.2
              034200 PST-INIT-002.                                                    SM2064.2
              034300     MOVE   +00005 TO WRK-DS-05V00-O005-001 OF GRP-002 (1).       SM2064.2
              034400     MOVE   +000000005 TO WRK-DS-09V00-901.                       SM2064.2
              034500 PST-TEST-002.                                                    SM2064.2
              034800     MOVE    "PSEUDO-TEXT/IDENTIFR" TO FEATURE.                   SM2064.2
              036100     COPY                                                    KP002SM2064.2
              036200             REPLACING == WRK-DS-09V00-901                        SM2064.2
              036300                          SUBTRACT 1 FROM                         SM2064.2
              036400                          WRK-DS-05V00-O005-001 IN GRP-002 (1)==  SM2064.2
              036500             BY         WRK-DS-05V00-O005-001 IN WRK-XN-00050-O005SM2064.2
              036600-                  F-001 IN GRP-006 IN GRP-004 IN GRP-002 IN GRP-0SM2064.2
              036700-                      01 (1).                                    SM2064.2
              059700 PST-TEST-009.                                                    SM2064.2
              059800     PERFORM FAIL.                                                SM2064.2
              059900     SUBTRACT 1 FROM ERROR-COUNTER.                               SM2064.2
              060900     COPY                                                   KP008 SM2064.2
              061000         REPLACING ==FAIL. THIS IS GARBAGE. SUBTRACT 1 FROM       SM2064.2
              061100                     ERROR-COUNTER. ==                            SM2064.2
              061200         BY  ==PASS. ==.                                          SM2064.2
              061400     IF P-OR-F IS EQUAL TO "FAIL*"  ADD 1 TO ERROR-COUNTER.       SM2064.2
              061500     GO TO PST-WRITE-009.                                         SM2064.2
              """,
            """
              000100 IDENTIFICATION DIVISION.                                         SM2064.2
              000200 PROGRAM-ID.                                                      SM2064.2
              000300     SM206A.                                                      SM2064.2
              021500 PROCEDURE DIVISION.                                              SM2064.2
              021600 CCVS1 SECTION.                                                   SM2064.2
              032800 CCVS1-EXIT.                                                      SM2064.2
              032900     EXIT.                                                        SM2064.2
              033000 SECT-SM206-0001 SECTION.                                         SM2064.2
              033800     COPY                                                    KP001SM2064.2
              033900             REPLACING ==PERFORM FAIL. == BY ====.                SM2064.2
              034100 SECT-SM206-0002 SECTION.                                         SM2064.2
              034200 PST-INIT-002.                                                    SM2064.2
              034300     MOVE   +00005 TO WRK-DS-05V00-O005-001 OF GRP-002 (1).       SM2064.2
              034400     MOVE   +000000005 TO WRK-DS-09V00-901.                       SM2064.2
              034500 PST-TEST-002.                                                    SM2064.2
              034800     MOVE    "PSEUDO-TEXT/IDENTIFR" TO FEATURE.                   SM2064.2
              036100     COPY                                                    KP002SM2064.2
              036200             REPLACING == WRK-DS-09V00-901                        SM2064.2
              036300                          SUBTRACT 1 FROM                         SM2064.2
              036400                          WRK-DS-05V00-O005-001 IN GRP-002 (1)==  SM2064.2
              036500             BY         WRK-DS-05V00-O005-001 IN WRK-XN-00050-O005SM2064.2
              036600-                  F-001 IN GRP-006 IN GRP-004 IN GRP-002 IN GRP-0SM2064.2
              036700-                      01 (1).                                    SM2064.2
              059700 PST-TEST-009.                                                    SM2064.2
              059800     PERFORM FAIL.                                                SM2064.2
              059900     SUBTRACT 1 FROM ERROR-COUNTER.                               SM2064.2
              060900     COPY                                                   ~~>KP008 SM2064.2
              061000         REPLACING ==FAIL. THIS IS GARBAGE. SUBTRACT 1 FROM       SM2064.2
              061100                     ERROR-COUNTER. ==                            SM2064.2
              061200         BY  ==PASS. ==.                                          SM2064.2
              061400     IF P-OR-F IS EQUAL TO "FAIL*"  ADD 1 TO ERROR-COUNTER.       SM2064.2
              061500     GO TO PST-WRITE-009.                                         SM2064.2
              """,
            spec -> spec.afterRecipe(cu -> {
                List<SearchResult> searchResults = new ArrayList<>(1);
                visitor.visit(cu, searchResults);
                Assertions.assertThat(searchResults).hasSize(1);
            }), true)
        );
    }
}
