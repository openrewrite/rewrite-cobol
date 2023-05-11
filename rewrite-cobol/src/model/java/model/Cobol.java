/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package model;

import generate.Skip;

public interface Cobol {

    @Skip
    class ArithmeticExpression {}
    @Skip
    class Subscript {}
    @Skip
    class CobolWord {}
    @Skip
    class QualifiedDataName {}
    @Skip
    class StatementPhrase {}
    @Skip
    class Parenthesized {}
    @Skip
    class Condition {}
    @Skip
    class ProcedureName {}
    @Skip
    class PictureString {}

    // We made the mistake of removing the model object code because code generation was very slow due to the number of AST elements.
    // Requires manually re-adding the model objects.
}
