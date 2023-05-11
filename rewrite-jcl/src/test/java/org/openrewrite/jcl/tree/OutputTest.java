/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.jcl.tree;

import org.junit.jupiter.api.Test;
import org.openrewrite.test.RewriteTest;

import static org.openrewrite.jcl.tree.ParserAssertions.jcl;

public class OutputTest implements RewriteTest {

    @Test
    void exec() {
        rewriteRun(
          jcl("//Name OUTPUT")
        );
    }

    @Test
    void parameterAssignment() {
        rewriteRun(
          jcl("//Name OUTPUT PGM=WT1")
        );
    }

    @Test
    void specialCharacters() {
        rewriteRun(
          jcl("//Name OUTPUT PGM='3400-6'")
        );
    }

    @Test
    void parensAssignment() {
        rewriteRun(
          jcl("//Name OUTPUT COND.PSTEP3=(4,GT,PSTEP1)")
        );
    }

    @Test
    void startsWithComma() {
        rewriteRun(
          jcl("//Name OUTPUT TIME=(,50)")
        );
    }

    @Test
    void multipleParameterTypes() {
        rewriteRun(
          jcl("//Name OUTPUT COND.PSTEP3=(4,GT,PSTEP1),RD=R")
        );
    }

    @Test
    void procParameter() {
        rewriteRun(
          jcl("//Name OUTPUT PROC=WRIT35")
        );
    }
}
