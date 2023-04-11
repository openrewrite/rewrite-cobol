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


// add delimiter:
/*
/* [comments]
xx [comments]
*/
jclStatement
    : JCL_STATEMENT (jobStatement | ddStatement | execStatement | outputStatement | procStatement | pendStatement)
    ;

/*
//jobname JOB [parameter [comments]]
// //jobname JOB
*/
jobStatement
    : JOB (parameter (COMMA parameter)*)? // comments ...
    ;

/*
//[ddname] DD [parameter [comments]]
//[ddname] DD
*/
ddStatement
    : DD (parameter (COMMA parameter)*)? // Add comments ...
    ;

/*
//[stepname] EXEC parameter [comments]
*/
execStatement
    : EXEC parameter // Add comments ...
    ;

outputStatement
    : OUTPUT (parameter+)?
    ;

/*
Using symbols as positional parameters
When a symbol is a positional parameter followed by other parameters in the statement, follow the
symbol with a period instead of a comma. For example:
//DS1 DD &POSPARM.DSNAME=ATLAS,DISP=OLD
If &POSPARM is nullified, the statement appears as:
//DS1 DD DSNAME=ATLAS,DISP=OLD
When assigning a substitution text to &POSPARM, include the comma:
POSPARM='DUMMY,'
Using two or more symbols in succession
Code two or more symbols in succession without including a comma. For example:
PARM=&DECK&CODE
*/

/*
//[name] PEND [comments]
*/
pendStatement
    : PEND // Add comments
    ;

/*
//[name] PROC [parameter [comments]]
//[name] PROC
*/
procStatement
    : PROC // Add parameters and comments ...
    ;

/*
//[name] SCHEDULE parameter [comments]
*/
scheduleStatement
    : SCHEDULE parameter // add comments
    ;

/*
//[name] SET parameter [comments]
*/
setStatement
    : SET parameter // add comments
    ;

/*
//[name] XMIT parameter[,parameter] [comments]
*/
xmitStatement
    : XMIT parameter // add comma delimited parameters and comments
    ;

parameter
    : PARAMETER
    | NAME_FIELD
    | parameterLiteral
    | parameterAssignment
    | parameterParentheses
    ;

parameterParentheses
    : L_PAREN parameter (COMMA parameter)* R_PAREN
    ;

parameterAssignment
    : (PARAMETER | NAME_FIELD) EQUAL parameter
    ;

// Force a separate visit on parameter literals since 'some , literal' may be comma separated.
parameterLiteral
    : PARAMETER_LITERAL
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

