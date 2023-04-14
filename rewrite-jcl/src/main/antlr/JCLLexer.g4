lexer grammar JCLLexer;

UTF_8_BOM : '\uFEFF' -> skip;

WS : [ \t\f;]+ -> channel(HIDDEN);
NEWLINE : EOL -> channel(HIDDEN);

CONTINUATION : ',' NEWLINE '//' -> channel(HIDDEN);

// statement identifiers
JCL_STATEMENT : '//' ~[*] NAME_FIELD?;

// JES3 and Comments.
UNSUPPORTED : '//*' -> pushMode(INSIDE_UNSUPPORTED);

JES2 : '/*' -> pushMode(INSIDE_JES2);


LF : '\n';
CR : '\r';
CRLF : CR LF;
FORM_FEED : '\u000C';
EOL : LF | CR | CRLF | FORM_FEED;

//OPERATION_FIELD
//    : // TODO add operation keywords.
//    ;

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


/* Info on parameter field ...
Parameter field
The parameter field consists of two types of parameters: positional parameters and keyword
parameters. All positional parameters must precede all keyword parameters. Keyword parameters follow
the positional parameters.
Commas: Use commas to separate positional parameters, keyword parameters, and subparameters in
the parameter field.
Positional Parameters: A positional parameter consists of:
• Characters that appear in uppercase in the syntax and must be coded as shown
• Variable information, or
• A combination.
For example, DATA on a DD statement, programmer's-name on a JOB statement, and PGM=programname
on an EXEC statement.
Code positional parameters first in the parameter field in the order shown in the syntax. If you omit a
positional parameter and code a following positional parameter, code a comma to indicate the omitted
parameter. Do not code the replacing comma if:
• The omitted positional parameter is the last positional parameter.
• All following positional parameters are also omitted.
• Only keyword parameters follow.
• All positional parameters are omitted.
Keyword parameters: A keyword consists of characters that appear in uppercase in the syntax and must
be coded as shown followed by an equals sign followed by either characters that must be coded as shown
or variable information. For example, RD=R and MSGCLASS=class-name on the JOB statement.
Code any of the keyword parameters for a statement in any order in the parameter field after the
positional parameters. Because of this positional independence, never code a comma to indicate the
absence of a keyword parameter.
Multiple subparameters: A positional parameter or the variable information in a keyword parameter
sometimes consists of more than one item, called a subparameter list. A subparameter list can consist of
both positional and keyword subparameters. These subparameters follow the same rules as positional
and keyword parameters.
When a parameter contains more than one subparameter, separate the subparameters by commas and
enclose the subparameter list in parentheses or, if indicated in the syntax, by apostrophes. If the list is a
single keyword subparameter or a single positional subparameter with no omitted preceding
subparameters, omit the parentheses or apostrophes.
Null positional subparameters: You are allowed to specify null (that is, omitted) positional subparameters
except where the Syntax section of a particular parameter states otherwise. (For example, null positional
subparameters are not allowed on a COND parameter of an EXEC statement or on an AMP parameter of a
DD statement.) You specify a null positi
*/
PARAMETER
    : ACCODE | ACCT | ADDRESS | ADDRSPC | AFF | AMP | AVGREC
    | BLKSIZE | BLKSZLIM | BUFND | BUFNI | BUFNO | BUFSP | BUILDING | BURST | BYTES
    | CCSID | CHARS | CHKPT | CKPTLINE | CKPTPAGE | CKPTSEC | CLASS | CNTL
    | COLORMAP | COMMAND | COMPACT | COMSETUP | COND | CONTROL | COPIES | CROPS
    | DATA | DATACK | DATACLAS | DCB | DDNAME | DEFAULT | DEN | DEPT | DEST | DISP | DLM
    | DPAGELBL | DSN | DSNTYPE | DSORG | DUMMY | DUPLEX | DYNAMNBR
    | ENDCNTL | EXEC | EXPDT
    | FCB | FILEDATA | FLASH | FORMDEF | FORMLEN | FORMS | FREE
    | GROUP | GROUPID
    | HOLD
    | IF | THEN | ELSE | ENDIF
    | INDEX
    | JCLLIB | JESDS | JOB | JOBCAT | JOBLIB
    | KEYOFF
    | LABEL | LGSTREAM | LIKE | LINDEX | LINECT | LINES | LRECL
    | MEMLIMIT | MGMTCLAS | MODIFY | MSGCLASS | MSGLEVEL
    | NAME | NOTIFY | NULLFILE
    | OFFSET | OPTCD | OUTBIN | OUTDISP | OUTLIM | OUTPUT | OVERLAY | OVFL
    | PAGEDEF | PAGES | PARM | PASSWORD | PATH | PATHDISP | PATHMODE | PATHOPTS | PEND
    | PERFORM | PGM | PIMSG | PRMODE | PROC | PROTECT | PRTERROR | PRTNO | PRTOPTNS | PRTQUEUE | PRTSP | PRTY
    | QNAME
    | RD | RECFM | RECORG | REF | REFDD | REGION | RESFMT | RESTART | RETAIN | RETRY | RETPD | RLS | ROOM
    | SCHENV | SECLABEL | SECMODEL | SEGMENT | SER | SORTCKPT | SPIN | SPACE | STEPCAT | STEPLIB
    | STORCLAS | STRNO | SUBSYS | SYNAD | SYMNAMES | SYSABEND | SYSAREA | SYSCHK | SYSCKEOV | SYSIN
    | SYSMDUMP | SYSOUT | SYSUDUMP
    | TERM | THRESHLD | TIME | TITLE | TRC | TRTCH | TYPRUN
    | UNIT | USER | USERDATA | USERLIB
    | VIO | VOL
    | WRITER
    | XMIT
    | ASTERISK
    ;

PARAMETER_LITERAL
    : SINGLEQUOTE .*? SINGLEQUOTE
    ;

// parameter names
ACCODE : A C C O D E;
ACCT : A C C T;
ADDRESS : A D D R E S S;
ADDRSPC : A D D R S P C;
AFF : A F F;
AMP : A M P;
AVGREC : A V G R E C;
BLKSIZE : B L K S I Z E;
BLKSZLIM : B L K S Z L I M;
BUFND : B U F N D;
BUFNI : B U F N I;
BUFNO : B U F N O;
BUFSP : B U F S P;
BUILDING : B U I L D I N G;
BURST : B U R S T;
BYTES : B Y T E S;
CCSID : C C S I D;
CHARS : C H A R S E T;
CHKPT : C H K P T;
CKPTLINE : C K P T L I N E;
CKPTPAGE : C K P T P A G E;
CKPTSEC : C K P T S E C;
CLASS : C L A S S;
COLORMAP : C O L O R M A P;
COMMAND : C O M M A N D;
COMPACT : C O M P A C T;
COMSETUP : C O M S E T U P;
COND : C O N D;
CONTROL : C O N T R O L;
COPIES : C O P I E S;
CROPS : C R O P S;
DATA : D A T A;
DATACK : D A T A C K;
DATACLAS : D A T A C L A S;
DCB : D C B;
DDNAME : D D N A M E;
DEFAULT : D E F A U L T;
DEN : D E N;
DEPT : D E P T;
DEST : D E S T;
DISP : D I S P;
DLM : D L M;
DPAGELBL : D P A G E L B L;
DSN : D S N;
DSNTYPE : D S N T Y P E;
DSORG : D S O R G;
DUMMY : D U M M Y;
DUPLEX : D U P L E X;
DYNAMNBR : D Y N A M N B R;
EXPDT : E X P D T;
FCB : F C B;
FILEDATA : F I L E D A T A;
FLASH : F L A S H;
FORMDEF : F O R M D E F;
FORMLEN : F O R M L E N;
FORMS : F O R M S;
FREE : F R E E;
GROUP : G R O U P;
GROUPID : G R O U P I D;
HOLD : H O L D;
INDEX : I N D E X;
JESDS : J E S D S;
JOBCAT : J O B C A T;
JOBLIB : J O B L I B;
KEYOFF : K E Y O F F;
LABEL : L A B E L;
LGSTREAM : L G S T R E A M;
LIKE : L I K E;
LINDEX : L I N D E X;
LINECT : L I N E C T;
LINES : L I N E S;
LRECL : L R E C L;
MEMLIMIT : M E M L I M I T;
MGMTCLAS : M G M T C L A S;
MODIFY : M O D I F Y;
MSGCLASS : M S G C L A S S;
MSGLEVEL : M S G L E V E L;
NAME : N A M E;
NULLFILE : N U L L F I L E;
OFFSET : O F F S E T;
OPTCD : O P T C D;
OUTBIN : O U T B I N;
OUTDISP : O U T D I S P;
OUTLIM : O U T L I M;
OVERLAY : O V E R L A Y;
OVFL : O V F L;
PAGEDEF : P A G E D E F;
PAGES : P A G E S;
PARM : P A R M;
PASSWORD : P A S S W O R D;
PATH : P A T H;
PATHDISP : P A T H D I S P;
PATHMODE : P A T H M O D E;
PATHOPTS : P A T H O P T S;
PERFORM : P E R F O R M;
PGM : P G M;
PIMSG : P I M S G;
PRMODE : P R M O D E;
PROTECT : P R O C T E C T;
PRTERROR : P R T E R R O R;
PRTNO : P R T N O;
PRTOPTNS : P R T O P T N S;
PRTQUEUE : P R T Q U E U E;
PRTSP : P R T S P;
PRTY : P R T Y;
QNAME : Q N A M E;
RD : R D;
RECFM : R E C F M;
RECORG : R E C O R G;
REF : R E F;
REFDD : R E F D D;
REGION : R E G I O N;
RESFMT : R E S F M T;
RESTART : R E S T A R T;
RETAIN : R E T A I N;
RETRY : R E T R Y;
RETPD : R E T P D;
RLS : R L S;
ROOM : R O O M;
SCHENV : S C H E N V;
SECLABEL : S E C L A B E L;
SECMODEL : S E C M O D E L;
SEGMENT : S E G M E N T;
SER : S E R;
SORTCKPT : S O R T C K P T;
SPIN : S P I N;
SPACE : S P A C E;
STEPCAT : S T E P C A T;
STEPLIB : S T E P L I B;
STORCLAS : S T O R C L A S;
STRNO : S T R N O;
SUBSYS : S U B S Y S;
SYNAD : S Y N A D;
SYMNAMES : S Y M N A M E S;
SYSABEND : S Y S A B E N D;
SYSAREA : S Y S A R E A;
SYSCHK : S Y S C H K;
SYSCKEOV : S Y S C K E O V;
SYSIN : S Y S I N;
SYSMDUMP : S Y S M D U M P;
SYSOUT : S Y S O U T;
SYSUDUMP : S Y S U D U M P;
TERM : T E R M;
THRESHLD : T H R E S H L D;
TIME : T I M E;
TITLE : T I T L E;
TRC : T R C;
TRTCH : T R T C H;
TYPRUN : T Y P R U N;
UNIT : U N I T;
USER : U S E R;
USERDATA : U S E R D A T A;
USERLIB : U S E R L I B;
VIO : V I O;
VOL : V O L;
WRITER : W R I T E R;

// symbols
EQUAL : '=';
L_BRACE : '{';
R_BRACE : '}';

L_BRACKET : '[';
R_BRACKET : ']';

L_PAREN : '(';
R_PAREN : ')';

AMPERSAND : '&';
ASTERISK : '*';
PLUS : '+';
MINUS : '-';

SINGLEQUOTE : '\'';
DOUBLEQUOTE : '"';

// names
NAME_FIELD : NAME_CHAR ((PERIOD NAME_CHAR)+)?;
PERIOD : '.';
COMMA : ',';
NAME_CHAR : ([a-zA-Z0-9$#@]+ | ASTERISK);

// add lexer rule for names
// JOB accounting
   //information
   //Hyphens (-) //JOBA JOB D58-D04
// JOB programmer's-name Hyphens (-), leading periods, or
   //embedded periods. Note that a trailing
   //period requires enclosing apostrophes.
   // //JOBB JOB ,S-M-TU
   // //JOBC JOB ,.ABC
   // //JOBD JOB ,P.F.M
   // //JOBE JOB ,'A.B.C.'

//EXEC ACCT Hyphens (-) or plus zero (+0, an
  //overpunch)
  // //S1 EXEC PGM=A,ACCT=D58-LOC
  // //S2 EXEC PGM=B,ACCT=D04+0

//DD DSNAME Hyphens (-) DSNAME=A-B-C

// Table 12. Special Characters that Do Not Require Enclosing Apostrophes (continued)
   //Statement and
   //parameter or
   //subparameter
   //Special characters not needing
   //enclosing apostrophes
   //Examples
   //Periods to indicate a qualified data set
   //name
   //DSNAME=A.B.C
   //Double ampersands to identify a
   //temporary data set name, and to
   //identify an in-stream or sysout data set
   //name
   //DSNAME=&&TEMPDS
   //DSNAME=&&PAYOUT
   //Parentheses to enclose the member
   //name of a partitioned data set (PDS) or
   //partitioned data set extended (PDSE) or
   //the generation number of a generation
   //data set
   //DSNAME=PDS1(MEMA)
   //DSNAME=ISDS(PRIME)
   //DSNAME=GDS(+1)
   //Plus (+) or minus (-) sign to identify a
   //generation of a generation data group
   //DSNAME=GDS(-2)
   //DD VOLUME=SER Hyphens (-) VOLUME=SER=PUB-RD
   //DD UNIT device-type Hyphens (-) UNIT=SYSDA


// Note: The system recognizes the following hexadecimal representations of the U.S. National characters;
//@ as X'7C'; $ as X'5B'; and # as X'7B'. In countries other than the U.S., the U.S. National characters
//represented on terminal keyboards might generate a different hexadecimal representation and cause an
//error. For example, in some countries the $ character may generate a X'4A'.

mode INSIDE_JES2;
JES2_NEWLINE : NEWLINE -> type(NEWLINE), channel(HIDDEN), popMode;

JES2_TEXT : ~[\r\n]+;

mode INSIDE_UNSUPPORTED;
UNSUPPORTED_NEWLINE : NEWLINE -> type(NEWLINE), channel(HIDDEN), popMode;

UNSUPPORTED_TEXT : ~[\r\n]+;

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