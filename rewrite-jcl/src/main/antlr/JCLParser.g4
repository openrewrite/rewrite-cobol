parser grammar JCLParser;

options { tokenVocab=JCLLexer; }

compilationUnit
    : statement* EOF
    ;

statement
    : jclStatement
    | unsupportedStatement
    ;

jclStatement
    : JCL_STATEMENT (jobStatement |ddStatement | execStatement | outputStatement | pendStatement | procStatement | setStatement | xmitStatement)
    ;

/* Syntax
//jobname JOB [parameter [comments]]
//jobname JOB
*/
jobStatement
    : JOB (COMMA? parameter)* // comments ...
    ;

/* Syntax
// [ddname ] DD [positional-parameter][,keyword-parameter]...[comments]
[procstepname.ddname]

// [ddname ] DD
[procstepname.ddname]
*/
ddStatement
    : DD (COMMA? parameter)* // Add comments ...
    ;

/* Syntax
//[stepname] EXEC positional-parm[,keyword-parm]...[,symbolic-parm=value]...
[comments]
*/
execStatement
    : EXEC (COMMA? parameter)* // Add comments ...
    ;

/* Syntax
//name OUTPUT parameter[,parameter]... [comments]
*/
outputStatement
    : OUTPUT (COMMA? parameter)* // Add comments ...
    ;

/* Syntax
//[name] PEND [comments]
The PEND statement consists of the characters // in columns 1 and 2 and three fields:
name, operation (PEND), and comments. Do not continue a PEND statement.
*/
pendStatement
    : PEND // Add comments
    ;

/* Syntax
//[name] PROC [parameter [comments]]
//[name] PROC
*/
procStatement
    : PROC (COMMA? parameter)* // Add comments ...
    ;

/* Syntax
//[name] SET symbolic-parameter=value [,symbolic-parameter=value]... [comments]
*/
setStatement
    : SET (COMMA? parameter)* // Add comments ...
    ;

/* Syntax
//[name] XMIT parameter[,parameter]... [comments]

The XMIT JCL statement consists of the characters // in columns 1 and 2 and four fields:
name, operation (XMIT), parameter, and comments.
*/
xmitStatement
    : XMIT (COMMA? parameter)* // Add comments ...
    ;

parameter
    : name
    | parameterLiteral
    | parameterAssignment
    | parameterParentheses
    ;

parameterParentheses
    : L_PAREN (COMMA? parameter)* R_PAREN
    ;

parameterAssignment
    : (PARAMETER | NAME_FIELD | EXEC | OUTPUT | PROC) EQUAL parameter
    ;

// Force a separate visit on parameter literals since 'some , literal' may be comma separated.
parameterLiteral
    : PARAMETER_LITERAL
    ;

name
    : (PARAMETER | NAME_FIELD) parameterParentheses?
    ;

unsupportedStatement
    : UNSUPPORTED UNSUPPORTED_TEXT?
    | JES2 JES2_TEXT?
    ;

//conditionStatement
//    : ifStatement
//    | elseStatement
//    | endifStatement
//    ;

//ifStatement
//    : IF [relational expression] THEN [comments]
//    ;
//
//elseStatement
//    : ELSE [comments]
//    ;
//
//endifStatement
//    : ENDIF [comments]
//    ;
//
//

// TODO: add statements with labels.
/*
//label CNTL [* comments]
//[label] ENDCNTL [comments]
//[label] EXPORT [comments]
*/

