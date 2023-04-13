parser grammar JCLParser;

options { tokenVocab=JCLLexer; }

compilationUnit
    : statement* EOF
    ;

statement
    : jclStatement
    ;

// add JCL COMMAND:
// // command [parameter] [comments]

// add command:
// //[name] COMMAND ‘command command-operand’ [comments]

// add comments:
// //* comments

/*
NOTE about comments:
JES3 control statement fields start with the SAME delimiter as COMMENTS ...

The rules for coding JES3 control statements are the same as the rules for JCL statements, with the
following additions:
• Columns 1, 2, and 3 generally contain the characters //* (slash-slash-asterisk). Some JES3 control
statements may contain, and certain other JES3 control statements must contain only a single slashasterisk
(/*) in columns 1 and 2.
• Columns 3 and 4 must not be blank.
• To code a comment on a JES3 control statement, code a blank after the control statement, and end the
comment before column 72. Columns 73-80 are ignored by z/OS and are typically used for sequence
numbers.
*/

jclStatement
    : JCL_STATEMENT (jobStatement |ddStatement | execStatement | outputStatement | pendStatement | procStatement | setStatement | xmitStatement)
    ;

/* Syntax
//jobname JOB [parameter [comments]]
//jobname JOB
*/
jobStatement
    : JOB (parameter (COMMA parameter)*)? // comments ...
    ;

/* Syntax
// [ddname ] DD [positional-parameter][,keyword-parameter]...[comments]
[procstepname.ddname]

// [ddname ] DD
[procstepname.ddname]
*/
ddStatement
    : DD (parameter (COMMA parameter)*)? // Add comments ...
    ;

/* Syntax
//[stepname] EXEC positional-parm[,keyword-parm]...[,symbolic-parm=value]...
[comments]
*/
execStatement
    : EXEC (parameter (COMMA parameter)*)? // Add comments ...
    ;

/* Syntax
//name OUTPUT parameter[,parameter]... [comments]
*/
outputStatement
    : OUTPUT (parameter (COMMA parameter)*)? // Add comments ...
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
    : PROC (parameter (COMMA parameter)*)? // Add comments ...
    ;

/* Syntax
//[name] SET symbolic-parameter=value [,symbolic-parameter=value]... [comments]
*/
setStatement
    : SET (parameter (COMMA parameter)*)? // Add comments ...
    ;

/* Syntax
//[name] XMIT parameter[,parameter]... [comments]

The XMIT JCL statement consists of the characters // in columns 1 and 2 and four fields:
name, operation (XMIT), parameter, and comments.
*/
xmitStatement
    : XMIT (parameter (COMMA parameter)*)? // Add comments ...
    ;

parameter
    : name
    | parameterLiteral
    | parameterAssignment
    | parameterParentheses
    ;

parameterParentheses
    : L_PAREN parameter? (COMMA parameter)* R_PAREN
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

