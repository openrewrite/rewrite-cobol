/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.jcl.tree;

import org.junit.jupiter.api.Test;
import org.openrewrite.test.RewriteTest;

import static org.openrewrite.jcl.tree.ParserAssertions.jcl;

public class CommentStatementTest implements RewriteTest {

    @Test
    void commentStatement() {
        rewriteRun(
          jcl("//* some comment")
        );
    }

    @Test
    void multipleComments() {
        rewriteRun(
          jcl(
            """
            //* THE COMMENT STATEMENT CANNOT BE CONTINUED,
            //* BUT IF YOU HAVE A LOT TO SAY, YOU CAN FOLLOW A
            //* COMMENT STATEMENT WITH MORE COMMENT
            //* STATEMENTS.
            """)
        );
    }
}
