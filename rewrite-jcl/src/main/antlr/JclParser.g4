parser grammar JclParser;

options { tokenVocab=JclLexer; }

startRule: compilationUnit;

compilationUnit
    : EOF
    ;