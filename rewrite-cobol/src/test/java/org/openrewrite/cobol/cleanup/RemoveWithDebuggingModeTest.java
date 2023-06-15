/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.cobol.cleanup;

import org.junit.jupiter.api.Test;
import org.openrewrite.cobol.CobolTest;
import org.openrewrite.test.RecipeSpec;

import static org.openrewrite.cobol.Assertions.cobol;

public class RemoveWithDebuggingModeTest extends CobolTest {

    @Override
    public void defaults(RecipeSpec spec) {
        spec.recipe(new RemoveWithDebuggingMode(true));
    }

    @Test
    public void noChange() {
        rewriteRun(
          cobol(getNistResource("CM101M.CBL"), true)
        );
    }

    @Test
    void removeWithDebuggingMode() {
        rewriteRun(
          cobol(
            """
                000100 IDENTIFICATION DIVISION.                                         DB1014.2
                000200 PROGRAM-ID.                                                      DB1014.2
                000300     DB101A.                                                      DB1014.2
                000400 ENVIRONMENT DIVISION.                                            DB1014.2
                000500 CONFIGURATION SECTION.                                           DB1014.2
                000600 SOURCE-COMPUTER.                                                 DB1014.2
                000700     XXXXX082                                                     DB1014.2
                000800         WITH DEBUGGING MODE.                                     DB1014.2
                000900 OBJECT-COMPUTER.                                                 DB1014.2
                001000     XXXXX083.                                                    DB1014.2
            """,
            """
                000100 IDENTIFICATION DIVISION.                                         DB1014.2
                000200 PROGRAM-ID.                                                      DB1014.2
                000300     DB101A.                                                      DB1014.2
                000400 ENVIRONMENT DIVISION.                                            DB1014.2
                000500 CONFIGURATION SECTION.                                           DB1014.2
                000600 SOURCE-COMPUTER.                                                 DB1014.2
                000700     XXXXX082.                                                    DB1014.2
                000800 OBJECT-COMPUTER.                                                 DB1014.2
                000900     XXXXX083.                                                    DB1014.2
           """
          )
        );
    }

    @Test
    void removeDebuggingMode() {
        rewriteRun(
          cobol(
            """
                000100 IDENTIFICATION DIVISION.                                         DB1014.2
                000200 PROGRAM-ID.                                                      DB1014.2
                000300     DB101A.                                                      DB1014.2
                000400 ENVIRONMENT DIVISION.                                            DB1014.2
                000500 CONFIGURATION SECTION.                                           DB1014.2
                000600 SOURCE-COMPUTER.                                                 DB1014.2
                000700     XXXXX082                                                     DB1014.2
                000800         DEBUGGING MODE.                                          DB1014.2
                000900 OBJECT-COMPUTER.                                                 DB1014.2
                001000     XXXXX083.                                                    DB1014.2
            """,
            """
                000100 IDENTIFICATION DIVISION.                                         DB1014.2
                000200 PROGRAM-ID.                                                      DB1014.2
                000300     DB101A.                                                      DB1014.2
                000400 ENVIRONMENT DIVISION.                                            DB1014.2
                000500 CONFIGURATION SECTION.                                           DB1014.2
                000600 SOURCE-COMPUTER.                                                 DB1014.2
                000700     XXXXX082.                                                    DB1014.2
                000800 OBJECT-COMPUTER.                                                 DB1014.2
                000900     XXXXX083.                                                    DB1014.2
            """
          )
        );
    }

    @Test
    void requiresAutoFormat() {
        rewriteRun(
            cobol(
              """
                  000100 IDENTIFICATION DIVISION.                                        \s
                  000200 PROGRAM-ID.                                                     \s
                  000300     CONTINUED.                                                  \s
                  000400 ENVIRONMENT DIVISION.                                           \s
                  000500 CONFIGURATION SECTION.                                          \s
                  000600 SOURCE-COMPUTER.                                                \s
                  000700                                                          XXXXX082SHIFTED
                  000800         WITH                                                    \s
                  000900         DEBUGGING                                               \s
                  001000         M                                                       \s
                  001100-         O                                                      \s
                  001200-          D                                                     \s
                  001300-           E.                                                   \s
              """,
              """
                  000100 IDENTIFICATION DIVISION.                                        \s
                  000200 PROGRAM-ID.                                                     \s
                  000300     CONTINUED.                                                  \s
                  000400 ENVIRONMENT DIVISION.                                           \s
                  000500 CONFIGURATION SECTION.                                          \s
                  000600 SOURCE-COMPUTER.                                                \s
                  000700                                                          XXXXX082SHIFTED
                  000800                                                                 \s
                  000900                                                                 \s
                  001000                                                                 \s
                  001100                                                                 \s
                  001200                                                                 \s
                  001300             .                                                   \s
              """
            )
        );
    }

    @Test
    void endOfCompilationUnit() {
        rewriteRun(
            cobol(
              """
                  000100 IDENTIFICATION DIVISION.                                        \s
                  000200 PROGRAM-ID.                                                     \s
                  000300     CONTINUED.                                                  \s
                  000400 ENVIRONMENT DIVISION.                                           \s
                  000500 CONFIGURATION SECTION.                                          \s
                  000600 SOURCE-COMPUTER.                                                \s
                  000700     XXXXX082                                                     SHIFTED
                  000800         WITH                                                    \s
                  000900         DEBUGGING                                               \s
                  001000         M                                                       \s
                  001100-         O                                                      \s
                  001200-          D                                                     \s
                  001300-           E.                                                   \s
              """, "" +
                 "000100 IDENTIFICATION DIVISION.                                         \n" +
                 "000200 PROGRAM-ID.                                                      \n" +
                 "000300     CONTINUED.                                                   \n" +
                 "000400 ENVIRONMENT DIVISION.                                            \n" +
                 "000500 CONFIGURATION SECTION.                                           \n" +
                 "000600 SOURCE-COMPUTER.                                                 \n" +
                 "000700     XXXXX082.                                                    SHIFTED\n\n"
            )
        );
    }

    @Test
    void isContinued() {
        rewriteRun(
          cobol(
            """
                000100 IDENTIFICATION DIVISION.                                        \s
                000200 PROGRAM-ID.                                                     \s
                000300     CONTINUED.                                                  \s
                000400 ENVIRONMENT DIVISION.                                           \s
                000500 CONFIGURATION SECTION.                                          \s
                000600 SOURCE-COMPUTER.                                                \s
                000700     XXXXX082                                                     SHIFTED
                000800         WITH                                                    \s
                000900         DEBUGGING                                               \s
                001000         M                                                       \s
                001100-         O                                                      \s
                001200-          D                                                     \s
                001300-           E.                                                   \s
                001400 OBJECT-COMPUTER.                                                \s
                001500     XXXXX083.                                                   \s
            """,
            """
                000100 IDENTIFICATION DIVISION.                                        \s
                000200 PROGRAM-ID.                                                     \s
                000300     CONTINUED.                                                  \s
                000400 ENVIRONMENT DIVISION.                                           \s
                000500 CONFIGURATION SECTION.                                          \s
                000600 SOURCE-COMPUTER.                                                \s
                000700     XXXXX082.                                                    SHIFTED
                000800 OBJECT-COMPUTER.                                                \s
                000900     XXXXX083.                                                   \s
            """
          )
        );
    }

    @Test
    void doNotUpdateSequenceAreas() {
        rewriteRun(
          spec -> spec.recipe(new RemoveWithDebuggingMode(false)),
          cobol(
            """
                000100 IDENTIFICATION DIVISION.                                        \s
                000200 PROGRAM-ID.                                                     \s
                000300     CONTINUED.                                                  \s
                000400 ENVIRONMENT DIVISION.                                           \s
                000500 CONFIGURATION SECTION.                                          \s
                000600 SOURCE-COMPUTER.                                                \s
                000700     XXXXX082                                                     SHIFTED
                000800         WITH                                                    \s
                000900         DEBUGGING                                               \s
                001000         M                                                       \s
                001100-         O                                                      \s
                001200-          D                                                     \s
                001300-           E.                                                   \s
                001400 OBJECT-COMPUTER.                                                \s
                001500     XXXXX083.                                                   \s
            """,
            """
                000100 IDENTIFICATION DIVISION.                                        \s
                000200 PROGRAM-ID.                                                     \s
                000300     CONTINUED.                                                  \s
                000400 ENVIRONMENT DIVISION.                                           \s
                000500 CONFIGURATION SECTION.                                          \s
                000600 SOURCE-COMPUTER.                                                \s
                000700     XXXXX082.                                                    SHIFTED
                001400 OBJECT-COMPUTER.                                                \s
                001500     XXXXX083.                                                   \s
            """
          )
        );
    }
}
