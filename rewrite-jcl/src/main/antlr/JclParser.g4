parser grammar JclParser;

options { tokenVocab=JclLexer; }

compilationUnit
    : statement* EOF
    ;

statement
    : jclStatement
    ;

jclStatement
    : JCL_STATEMENT (jobStatement || ddStatement)
    ;

jobStatement
    : JOB
    ;

ddStatement
    : DD
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
//pendStatement
//    : PEND [comments]
//    ;
//
//procStatement
//    : PROC ([parameter[comments]])?
//    ;

//scheduleStatement
//    : SCHEDULE parameter [comments]?
//    ;
//
//setStatement
//    : SET parameter [comments]
//    ;
//
//xmitStatement
//    : XMIT parameter[,parameter][comments]]
//    ;