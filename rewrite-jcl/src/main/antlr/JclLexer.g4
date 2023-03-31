lexer grammar JclLexer;

UTF_8_BOM : '\uFEFF' -> skip;

WS : [ \t]+ -> skip;

LF : '\n';
CR : '\r';
CRLF : CR LF;
FORM_FEED : '\u000C';
EOL : LF | CR | CRLF | FORM_FEED;

COMMENT : '//*' .*? EOL;

JCL_STATEMENT : '//';
JECL_STATEMENT : '/*' ('$' || JOBPARM || MESSAGE || NETACCT || NOTIFY || OUTPUT || PRIORITY || ROUTE || SETUP);
DELIMITER : '/*';

// keywords
CNTL : C N T L;
DATASET : D A T A S E T;
DD : D D;
ELSE : E L S E;
ENDCNTL : E N D C N T L;
ENDDATASET : E N D D A T A S E T;
ENDIF : E N D I F;
ENDPROCESS : E N D P R O C E S S;
EXEC : E X E C;
EXPORT : E X P O R T;
FORMAT : F O R M A T;
IF : I F;
INCLUDE : I N C L U D E;
JCLLIB : J C L L I B;
JOB : J O B;
JOBPARM : J O B P A R M;
MAIN : M A I N;
MESSAGE : M E S S A G E;
NET : N E T;
NETACCT : N E T A C C T;
NOTIFY : N O T I F Y;
OPERATOR : O P E R A T O R;
OUTPUT : O U T P U T;
PAUSE : P A U S E;
PEND : P E N D;
PRIORITY : P R I O R I T Y;
PROC : P R O C;
PROCESS : P R O C E S S;
ROUTE : R O U T E;
SCHEDULE : S C H E D U L E;
SET : S E T;
SETUP : S E T U P;
SIGNOFF : S I G N O F F;
SIGNON : S I G N O N;
THEN : T H E N;
XEQ : X E Q;
XMIT : X M I T;

// case insensitive chars
fragment A:('a'|'A');
fragment B:('b'|'B');
fragment C:('c'|'C');
fragment D:('d'|'D');
fragment E:('e'|'E');
fragment F:('f'|'F');
fragment G:('g'|'G');
fragment H:('h'|'H');
fragment I:('i'|'I');
fragment J:('j'|'J');
fragment K:('k'|'K');
fragment L:('l'|'L');
fragment M:('m'|'M');
fragment N:('n'|'N');
fragment O:('o'|'O');
fragment P:('p'|'P');
fragment Q:('q'|'Q');
fragment R:('r'|'R');
fragment S:('s'|'S');
fragment T:('t'|'T');
fragment U:('u'|'U');
fragment V:('v'|'V');
fragment W:('w'|'W');
fragment X:('x'|'X');
fragment Y:('y'|'Y');
fragment Z:('z'|'Z');