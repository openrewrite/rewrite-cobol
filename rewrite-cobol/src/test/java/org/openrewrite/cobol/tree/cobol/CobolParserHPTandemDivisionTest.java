/*
 * For commercial customers of Moderne Inc., this repository is licensed per the terms of our contract.
 * For everyone else, this is licensed under Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International.
 * See: https://creativecommons.org/licenses/by-nc-sa/4.0/
 */
package org.openrewrite.cobol.tree.cobol;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.openrewrite.cobol.CobolParser;
import org.openrewrite.cobol.CobolTest;
import org.openrewrite.cobol.internal.CobolDialect;
import org.openrewrite.test.RecipeSpec;

import static org.openrewrite.cobol.Assertions.cobol;

@Disabled("Requires slight modifications to column area detection")
class CobolParserHPTandemDivisionTest extends CobolTest {

    @Override
    public void defaults(RecipeSpec spec) {
        spec.parser(CobolParser.builder().setEnableCopy(false).setEnableReplace(false).setCobolDialect(CobolDialect.hpTandem()));
    }

    @Test
    void helloWorld() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION  DIVISION .
               PROGRAM-ID    . HELLO     .
               PROCEDURE DIVISION.
               DISPLAY 'Hello world!'.
               STOP RUN.
              """
          )
        );
    }

    @Test
    void arithmetic() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION .
               PROGRAM-ID . HELLO-WORLD .
               DATA DIVISION .
                   WORKING-STORAGE SECTION .
                       77 X PIC 99.
                       77 Y PIC 99.
                       77 Z PIC 99.
               PROCEDURE DIVISION .
                   SET X TO 10 .
                   SET Y TO 25 .
                   ADD X Y GIVING Z .
                   DISPLAY "X + Y = "Z .
               STOP RUN .
              """
          )
        );
    }

    @Test
    void environmentDivision() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID.
                   IC109A.
               ENVIRONMENT DIVISION.
               CONFIGURATION SECTION.
               SOURCE-COMPUTER.
                   XXXXX082.
               OBJECT-COMPUTER.
                   XXXXX083
                   MEMORY SIZE XXXXX068 CHARACTERS
                   PROGRAM COLLATING SEQUENCE IS COLLATING-SEQ-1.
               SPECIAL-NAMES.
                   ALPHABET PRG-COLL-SEQ IS
                   STANDARD-2.
               INPUT-OUTPUT SECTION.
               FILE-CONTROL. SELECT OPTIONAL IDENTIFIER ASSIGN TO DISK.
               I-O-CONTROL. IDENTIFIER.
               RERUN ON IDENTIFIER EVERY 10 RECORDS
               SAME RECORD AREA FOR IDENTIFIER
               MULTIPLE FILE TAPE CONTAINS IDENTIFIER POSITION 10
               COMMITMENT CONTROL FOR IDENTIFIER.
              """
          )
        );
    }


    @Test
    void inputOutputSection() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID.
                   IC109A.
               ENVIRONMENT DIVISION.
               INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                   SELECT PRINT-FILE ASSIGN TO
                       XXXXX055.
                   SELECT SEQ-FILE ASSIGN TO
                       XXXXX014.
                   SELECT SEQ-FILE RESERVE NO ALTERNATE AREA.
                   SELECT SEQ-FILE ORGANIZATION IS RECORD BINARY INDEXED.
                   SELECT SEQ-FILE PADDING CHARACTER IS IDENTIFIER IN IDENTIFIER.
                   SELECT SEQ-FILE RECORD DELIMITER IS STANDAR-1.
                   SELECT SEQ-FILE ACCESS MODE IS SEQUENTIAL.
                   SELECT SEQ-FILE RECORD KEY IS IDENTIFIER IN IDENTIFIER PASSWORD IS IDENTIFIER WITH DUPLICATES.
                   SELECT SEQ-FILE ALTERNATE RECORD KEY IS IDENTIFIER IN IDENTIFIER PASSWORD IS IDENTIFIER WITH DUPLICATES.
                   SELECT SEQ-FILE FILE STATUS IS IDENTIFIER IN IDENTIFIER IDENTIFIER IN IDENTIFIER.
                   SELECT SEQ-FILE RELATIVE KEY IS IDENTIFIER IN IDENTIFIER.
              """
          )
        );
    }


    @Test
    void procedureDivision() {
        rewriteRun(
          cobol(
            """
              *
               IDENTIFICATION  DIVISION .
               PROGRAM-ID    . HELLO     .
               PROCEDURE DIVISION USING GRP-01 GIVING dataName.
               DECLARATIVES.
               sectionName SECTION 77.
               USE GLOBAL AFTER STANDARD ERROR PROCEDURE ON INPUT.
               END DECLARATIVES.
              """
          )
        );
    }

    @Test
    void divisionUsing() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION  DIVISION .
               PROGRAM-ID    . HELLO     .
               PROCEDURE DIVISION USING GRP-01.
               STOP RUN.
              """
          )
        );
    }

    @Test
    void ic109a() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID.
                   IC109A.
               ENVIRONMENT DIVISION.
               CONFIGURATION SECTION.
               SOURCE-COMPUTER.
                   XXXXX082.
               OBJECT-COMPUTER.
                   XXXXX083.
               INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                   SELECT PRINT-FILE ASSIGN TO
                   XXXXX055.
               DATA DIVISION.
               FILE SECTION.
               FD  PRINT-FILE.
               01  PRINT-REC PICTURE X(120).
               01  DUMMY-RECORD PICTURE X(120).
               WORKING-STORAGE SECTION.
               77  WS1 PICTURE X.
               LINKAGE SECTION.
               01  GRP-01.
                   02  SUB-CALLED.
                       03  DN1  PICTURE X(6).
                       03  DN2  PICTURE X(6).
                       03  DN3  PICTURE X(6).
                   02  TIMES-CALLED.
                       03  DN4  PICTURE S999.
                       03  DN5  PICTURE S999.
                       03  DN6  PICTURE S999.
                   02  SPECIAL-FLAGS.
                       03  DN7 PICTURE X.
                       03  DN8 PICTURE X.
                       03  DN9 PICTURE X.
               PROCEDURE DIVISION USING GRP-01.
               SECT-IC109-0001 SECTION.
               PARA-IC109.
                   MOVE "IC109A" TO DN1.
                   MOVE SPACE TO WS1.
                   CALL "IC110A" USING WS1 GRP-01.
                   ADD 1 TO DN4.
                   MOVE WS1 TO DN9.
               EXIT-IC109.
                   EXIT PROGRAM.
               END-OF
              """
          )
        );
    }


    @Test
    void moveStatement() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. MOVETEST.
               DATA DIVISION.
               PROCEDURE DIVISION USING GRP-01.
               PARA-MOVETEST.
                   MOVE "MOVETEST" TO DN1.
                   MOVE SPACE TO WS1.
            """
          )
        );
    }

    @Test
    void mergeStatement() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. MERGETEST.
               PROCEDURE DIVISION.
               MERGE-TEST.
                   MERGE ST-FS4  ON ASCENDING KEY SORT-KEY
                       USING  SQ-FS1  SQ-FS2
                       OUTPUT PROCEDURE IS MERGE-OUTPUT-PROC.
              """
          )
        );
    }

    @Test
    void multiplyStatement() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. MULTIPLYTEST.
               PROCEDURE DIVISION.
               MULTIPLY -1.3 BY MULT4 ROUNDED.
            """
          )
        );
    }

    @Test
    void openStatement() {
        rewriteRun(
          cobol(
            """
              * Comments
               IDENTIFICATION DIVISION.
               PROGRAM-ID. OPENTEST.
               PROCEDURE DIVISION.
               OPEN OUTPUT SQ-FS2.
               OPEN INPUT TFIL REVERSED.
               OPEN INPUT TFIL WITH NO REWIND.
              """
          )
        );
    }

    @Test
    void performStatement() {
        rewriteRun(
          cobol(
            """
            * Comment
             IDENTIFICATION DIVISION.
             PROGRAM-ID. PARSERTEST.
             PROCEDURE DIVISION.
             PERFORM ST301M-MERGE THRU ST301M-SORT 1 TIMES.
            """
          )
        );
    }

    @Test
    void readStatement() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. READTEST.
               PROCEDURE DIVISION.
               READ SQ-FS3 END .
              """
          )
        );
    }

    @Test
    void receiveStatement() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. MERGETEST.
               PROCEDURE DIVISION.
               RECEIVE CM-INQUE-1 MESSAGE INTO MSG-72
                   NO DATA.
            """
          )
        );
    }

    @Test
    void fileSection() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID.
                   IC109A.
               DATA DIVISION.
               FILE SECTION.
               FD  PRINT-FILE.
               IS EXTERNAL.
               IS GLOBAL.
               BLOCK CONTAINS 1 TO 10 RECORDS.
               RECORD CONTAINS 10 CHARACTERS.
               RECORD IS VARYING IN SIZE FROM 1 TO 10 CHARACTERS DEPENDING ON IDENTIFIER IN IDENTIFIER.
               RECORD CONTAINS 1 TO 10 CHARACTERS.
               LABEL RECORD IS OMITTED.
               VALUE OF IDENTIFIER IS 10.
               LINAGE IS 10 LINES WITH FOOTING AT 10.
               LINAGE IS 10 LINES AT TOP 10.
               LINAGE IS 10 LINES AT BOTTOM 10.
               CODE-SET IS IDENTIFIER.
               RECORDING MODE IS IDENTIFIER.
               DATA RECORD IS IDENTIFIER.
               REPORT IS IDENTIFIER.
               01  PRINT-REC PICTURE X(120).
               01  DUMMY-RECORD PICTURE X(120).
              """
          )
        );
    }

    @Test
    void linkageSection() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
                   PROGRAM-ID.
                       IC109A.
                   DATA DIVISION.
                   LINKAGE SECTION.
                   01  GRP-01.
                       02  SUB-CALLED.
                           03  DN1  PICTURE X(6).
                           03  DN2  PICTURE X(6).
                           03  DN3  PICTURE X(6).
                       02  TIMES-CALLED.
                           03  DN4  PICTURE S999.
                           03  DN5  PICTURE S999.
                           03  DN6  PICTURE S999.
                       02  SPECIAL-FLAGS.
                           03  DN7 PICTURE X.
                           03  DN8 PICTURE X.
                           03  DN9 PICTURE X.
              """
          )
        );
    }

    @Test
    void localStorageSection() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. LocalStorage.
               DATA DIVISION.
               LOCAL-STORAGE Section.
               01  NUM  PIC 9(4).
              """
          )
        );
    }

    @Test
    void dataBaseSection() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. DBSection.
               DATA DIVISION.
               DATA-BASE SECTION.
               01 TRUE INVOKE TRUE
              """
          )
        );
    }

    @Test
    void screenSection() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. DBSection.
               DATA DIVISION.
               SCREEN SECTION.
               01 SCREEN1 BLANK LINE
               BELL
               BLINK
               ERASE EOL
               HIGHLIGHT
               GRID
               UNDERLINE
               SIZE IS IDENTIFIER IN IDENTIFIER
               LINE NUMBER IS PLUS IDENTIFIER IN IDENTIFIER
               COLUMN NUMBER IS PLUS IDENTIFIER IN IDENTIFIER
               FOREGROUND-COLOR IS IDENTIFIER IN IDENTIFIER
               BACKGROUND-COLOR IS IDENTIFIER IN IDENTIFIER
               CONTROL IS IDENTIFIER IN IDENTIFIER
               VALUE IS 10
               PICTURE IS ${'$'}(10)
               FROM IDENTIFIER IN IDENTIFIER TO IDENTIFIER IN IDENTIFIER
               USING IDENTIFIER IN IDENTIFIER
               USAGE IS DISPLAY
               BLANK WHEN ZERO
               JUSTIFIED RIGHT
               SIGN IS LEADING SEPARATE CHARACTER
               AUTO
               SECURE
               REQUIRED
               PROMPT CHARACTER IS IDENTIFIER IN IDENTIFIER OCCURS 01 TIMES
               FULL
               ZERO-FILL
               .
              """)
        );
    }

    @Disabled("Potential lexer issue: The REVERSE-VIDEO token maps to RESERVE-VIDEO")
    @Test
    void reverseVideo() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. DBSection.
               DATA DIVISION.
               SCREEN SECTION.
               01 REVERSE-VIDEO.
              """)
        );
    }

    @Test
    void acceptStatement() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. acceptStatement.
               PROCEDURE DIVISION.
               PARAGRAPH_NAME.
               ACCEPT identifier FROM DATE YYYYMMDD END-ACCEPT
               ACCEPT identifier FROM ESCAPE KEY
               ACCEPT identifier FROM mnemonicName
               ACCEPT identifier MESSAGE COUNT.
            """
          )
        );
    }

    @Test
    void alterStatement() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION .
               PROGRAM-ID . HELLO-WORLD .
               PROCEDURE DIVISION .
               ALTER PARA-54 TO PROCEED TO PARA-54B.
               ALTER PARA-23 TO PARA-24.
              """
          )
        );
    }


    @Test
    void cancelStatement() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. acceptStatement.
               PROCEDURE DIVISION.
               PARAGRAPH_NAME.
               CANCEL "literal"
               CANCEL identifier
               CANCEL libraryName BYTITLE.
              """
          )
        );
    }

    @Test
    void closeStatement() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. acceptStatement.
               PROCEDURE DIVISION.
               PARAGRAPH_NAME.
               CLOSE fileName UNIT FOR REMOVAL WITH LOCK
               CLOSE fileName WITH NO REWIND
               CLOSE fileName NO WAIT USING CLOSE-DISPOSITION OF ABORT
               CLOSE fileName NO WAIT USING ASSOCIATED-DATA identifier
               CLOSE fileName NO WAIT USING ASSOCIATED-DATA-LENGTH OF identifier.
              """
          )
        );
    }

    @Test
    void rewriteStatement() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. acceptStatement.
               PROCEDURE DIVISION.
               PARAGRAPH_NAME.
               REWRITE dataName IN fileName END-REWRITE.
              """
          )
        );
    }

    @Test
    void callStatement() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. acceptStatement.
               PROCEDURE DIVISION USING GRP-01.
               SECT-IC109-0001 SECTION.
               PARA-IC109.
                   CALL "IC110A" USING BY REFERENCE WS1 GRP-01.
                   CALL "IC110A" USING BY VALUE ADDRESS OF GRP-01.
                   CALL "IC110A" USING BY CONTENT LENGTH OF GRP-01.
                   CALL "IC110A" GIVING GRP-01.
                   CALL "IC110A" ON OVERFLOW CONTINUE.
                   CALL "IC110A" ON EXCEPTION CONTINUE.
                   CALL "IC110A" NOT ON EXCEPTION CONTINUE.
              """
          )
        );
    }

    @Test
    void writeStatement() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. acceptStatement.
               PROCEDURE DIVISION USING GRP-01.
               PARA-IC109.
                   WRITE IC110A FROM GRP-01.
                   WRITE IC110A BEFORE ADVANCING PAGE.
                   WRITE IC110A BEFORE ADVANCING 10 LINES.
                   WRITE IC110A BEFORE ADVANCING GRP-01.
                   WRITE IC110A AT END-OF-PAGE CONTINUE.
                   WRITE IC110A NOT AT END-OF-PAGE CONTINUE.
                   WRITE IC110A INVALID KEY CONTINUE.
                   WRITE IC110A NOT INVALID KEY CONTINUE.
              """
          )
        );
    }

    @Test
    void computeStatement() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION .
               PROGRAM-ID . HELLO-WORLD .
               PROCEDURE DIVISION .
                   COMPUTE V = (1 + 2) .
                   COMPUTE LAG-TIME =
                       ((SYS-HRS * 3600) + (SYS-MINS * 60) + SYS-SECS) -
                       ((HOURS OF MSG-TIME * 3600) + (MINUTES OF MSG-TIME * 60)
                       + SECONDS OF MSG-TIME)
                       END-COMPUTE .
              """
          )
        );
    }

    @Test
    void divideStatement() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. acceptStatement.
               PROCEDURE DIVISION USING GRP-01.
               SIG-TEST-GF-5-0.
                   DIVIDE 0.097 INTO DIV7 ROUNDED.
                   DIVIDE 0.097 INTO DIV7 GIVING DIV8 ROUNDED.
                   DIVIDE 0.097 BY DIV7 GIVING DIV8 ROUNDED.
                   DIVIDE 0.097 INTO DIV7 REMAINDER DIV9.
                   DIVIDE 0.097 INTO DIV7 ON SIZE ERROR CONTINUE.
                   DIVIDE 0.097 INTO DIV7 NOT ON SIZE ERROR CONTINUE.
              """
          )
        );
    }

    @Test
    void evaluateStatement() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. acceptStatement.
               PROCEDURE DIVISION USING GRP-01.
               F-ANNUITY-02.
               EVALUATE IC110A END-EVALUATE.
               EVALUATE IC110A ALSO IC110B.
               EVALUATE IC110A
               WHEN ANY ALSO ANY
                   CONTINUE
               WHEN IDENTIFIER THRU IDENTIFIER
                   CONTINUE
               WHEN TRUE
                   CONTINUE
               WHEN OTHER
                   CONTINUE.
              """
          )
        );
    }

    @Test
    void conditions() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. acceptStatement.
               PROCEDURE DIVISION USING GRP-01.
               F-ANNUITY-02.
               EVALUATE IC110A
               WHEN IDENTIFIER IS NOT ALPHABETIC-LOWER
                   CONTINUE
               WHEN IDENTIFIER IN IDENTIFIER
                   CONTINUE.
              """
          )
        );
    }

    @Test
    void conditionNameSubscriptReference() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. acceptStatement.
               PROCEDURE DIVISION.
               COMMA-SUBSCRIPT-TEST.
               EVALUATE NOT IDENTIFIER (IDENTIFIER, IDENTIFIER IDENTIFIER)
               .
              """
          )
        );
    }

    @Test
    void sendStatement() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. acceptStatement.
               PROCEDURE DIVISION.
               SEND CM-OUTQUE-1 FROM MSG-70 WITH EMI
                   AFTER ADVANCING PAGE.
              """
          )
        );
    }

    @Test
    void tableCallTest() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. acceptStatement.
               PROCEDURE DIVISION.
               COMMA-SUBSCRIPT-TEST.
               EVALUATE SUBSCRIPT
               WHEN IDENTIFIER (IDENTIFIER, IDENTIFIER IDENTIFIER)
                   CONTINUE.
              """
          )
        );
    }

    @Test
    void voidctionCallTest() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. acceptStatement.
               PROCEDURE DIVISION.
               COMMA-SUBSCRIPT-TEST.
               EVALUATE SUBSCRIPT
               WHEN IDENTIFIER (FUNCTION INTEGER (IDENTIFIER, IDENTIFIER IDENTIFIER) (1: 10))
                   CONTINUE.
              """
          )
        );
    }

    @Test
    void relationConditions() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. acceptStatement.
               PROCEDURE DIVISION.
               F-ANNUITY-02.
               EVALUATE IC110A
               WHEN NOT +IDENTIFIER IS NOT ZERO
               WHEN NOT +IDENTIFIER IS GREATER OR EQUAL +IDENTIFIER
               WHEN NOT +ZERO GREATER THAN (IDENTIFIER AND IDENTIFIER OR IDENTIFIER)
               .
              """
          )
        );
    }

    @Test
    void multiElementLiteral() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. acceptStatement.
               PROCEDURE DIVISION.
               Literal-Test.
               EVALUATE DFHRESP (IDENTIFIER).
              """
          )
        );
    }

    @Test
    void multiElementIdentifier() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. acceptStatement.
               PROCEDURE DIVISION.
               Identifier-Test.
               EVALUATE IDENTIFIER IN IDENTIFIER.
              """
          )
        );
    }

    @Test
    void openMultipleStatements() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. acceptStatement.
               PROCEDURE DIVISION.
               OPEN-FILES.
                   OPEN     INPUT IDENTIFIER REVERSED INPUT IDENTIFIER REVERSED
                   OPEN     OUTPUT IDENTIFIER WITH NO REWIND IDENTIFIER WITH NO REWIND
                   OPEN     I-O IDENTIFIER IDENTIFIER I-O IDENTIFIER IDENTIFIER
                   OPEN     EXTEND IDENTIFIER IDENTIFIER EXTEND IDENTIFIER IDENTIFIER.
              """
          )
        );
    }

    @Test
    void outOfOrderOpenStatements() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. acceptStatement.
               PROCEDURE DIVISION.
               OPEN-FILES.
                   OPEN     INPUT IDENTIFIER OUTPUT IDENTIFIER INPUT IDENTIFIER OUTPUT IDENTIFIER.
              """
          )
        );
    }

    @Test
    void unstringStatement() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. acceptStatement.
               PROCEDURE DIVISION.
               UNSTRING-TEST.
                   UNSTRING IDENTIFIER DELIMITED BY ALL IDENTIFIER OR ALL IDENTIFIER INTO IDENTIFIER DELIMITER IN IDENTIFIER COUNT IN IDENTIFIER END-UNSTRING
                   UNSTRING IDENTIFIER INTO IDENTIFIER WITH POINTER IDENTIFIER
                   UNSTRING IDENTIFIER INTO IDENTIFIER TALLYING IN IDENTIFIER
                   UNSTRING IDENTIFIER INTO IDENTIFIER ON OVERFLOW
                   UNSTRING IDENTIFIER INTO IDENTIFIER NOT ON OVERFLOW.
              """
          )
        );
    }

    @Test
    void terminateStatement() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. terminateStatement.
               PROCEDURE DIVISION.
               RW301M-CONTROL.
                   TERMINATE RFIL2.
              """
          )
        );
    }

    @Test
    void generateStatement() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. terminateStatement.
               PROCEDURE DIVISION.
               RW301M-CONTROL.
                   GENERATE RREC.
              """
          )
        );
    }

    @Test
    void subtractStatement() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. subtractStatement.
               PROCEDURE DIVISION.
               RW301M-CONTROL.
                   SUBTRACT 1 FROM ERROR-COUNTER
                   SUBTRACT N-10 FROM 0 GIVING N-19
                   SUBTRACT CORRESPONDING IDENTIFIER FROM IDENTIFIER ROUNDED.
              """
          )
        );
    }

    @Test
    void exitStatement() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. exitStatement.
               PROCEDURE DIVISION.
               RW301M-CONTROL.
                   EXIT PROGRAM.
              """
          )
        );
    }

    @Test
    void sortStatement() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. sortStatement.
               PROCEDURE DIVISION.
               SORT-STATEMENT.
                   SORT SORTFILE-1B
                       ON DESCENDING KEY KEY-1
                       ON ASCENDING KEY KEY-2
                       ON DESCENDING KEY KEY-3
                       ASCENDING KEY-4 KEY-5
                   USING SORTIN-1B
                   GIVING SORTOUT-1B.
              """
          )
        );
    }

    @Test
    void stringStatement() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. subtractStatement.
               PROCEDURE DIVISION.
               RW301M-CONTROL.
                   STRING NONNUMERICLITERAL, NONNUMERICLITERAL NONNUMERICLITERAL DELIMITED BY SIZE
                   INTO IDENTIFIER
                   WITH POINTER IDENTIFIER END-STRING
               .
              """
          )
        );
    }

    @Test
    void startStatement() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. subtractStatement.
               PROCEDURE DIVISION.
               STA-TEST-GF-01.
                   START IX-FS2 KEY IS NOT LESS THAN IDENTIFIER IN IDENTIFIER END-START.
               .
              """
          )
        );
    }

    @Test
    void goToStatement() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. subtractStatement.
               PROCEDURE DIVISION.
               ACCEPT-TEST-01.
                   GO TO CM105-FINI.
                   GO TO CM105-FINI DEPENDING ON IDENTIFIER IN IDENTIFIER.
               .
              """
          )
        );
    }

    @Test
    void ifStatement() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. subtractStatement.
               PROCEDURE DIVISION.
               IF--TEST-GF-99.
                   IF      ZERO IS EQUAL TO IF-D1
                      THEN PERFORM PASS
                   ELSE
                       PERFORM FAIL.
               .
              """
          )
        );
    }

    @Test
    void initializeStatement() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. subtractStatement.
               PROCEDURE DIVISION.
               INI-TEST-GF-1-0.
                   INITIALIZE IDENTIFIER IN IDENTIFIER REPLACING NATIONAL DATA BY 42.
               .
            """
          )
        );
    }

    @Test
    void initiateStatement() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. subtractStatement.
               PROCEDURE DIVISION.
               RW301M-CONTROL.
                   INITIATE RFIL2.
               .
              """
          )
        );
    }

    @Test
    void inspectStatement() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. subtractStatement.
               PROCEDURE DIVISION.
               RW301M-CONTROL.
                   INSPECT IDENTIFIER IN IDENTIFIER TALLYING IDENTIFIER IN IDENTIFIER FOR CHARACTER BEFORE INITIAL 42
                   INSPECT IDENTIFIER IN IDENTIFIER REPLACING CHARACTER BY IDENTIFIER IN IDENTIFIER BEFORE INITIAL 42
                   INSPECT IDENTIFIER IN IDENTIFIER TALLYING IDENTIFIER IN IDENTIFIER FOR CHARACTER BEFORE IDENTIFIER IN IDENTIFIER REPLACING ALL IDENTIFIER IN IDENTIFIER BY IDENTIFIER IN IDENTIFIER
                   INSPECT IDENTIFIER IN IDENTIFIER CONVERTING IDENTIFIER IN IDENTIFIER TO IDENTIFIER IN IDENTIFIER BEFORE IDENTIFIER IN IDENTIFIER
               .
              """
          )
        );
    }

    @Test
    void communicationSection() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. communicationSection.
               DATA DIVISION.
               COMMUNICATION SECTION.
                   CD COMMNAME FOR INITIAL INPUT.
                   CD COMMNAME FOR OUTPUT.
                   CD COMMNAME FOR INITIAL I-O.
              """
          )
        );
    }

    @Test
    void reportSection() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. communicationSection.
               DATA DIVISION.
               REPORT SECTION.
                   RD IDENTIFIER IN IDENTIFIER IS GLOBAL.
                   10 IDENTIFIER LINE NUMBER IS 10 ON NEXT PAGE.
              """
          )
        );
    }

    @Test
    void programLibrarySection() {
        rewriteRun(
          cobol(
            """
              * Comment
               IDENTIFICATION DIVISION.
               PROGRAM-ID. communicationSection.
               DATA DIVISION.
               PROGRAM-LIBRARY SECTION.
                   LD IDENTIFIER EXPORT ATTRIBUTE SHARING IS DONTCARE ENTRY-PROCEDURE IDENTIFIER FOR ZERO
                   LB IDENTIFIER IMPORT IS GLOBAL IS COMMON ATTRIBUTE
                   FUNCTIONNAME IS ZERO LIBACCESS IS BYFUNCTION LIBPARAMETER IS ZERO
              """
          )
        );
    }
}
