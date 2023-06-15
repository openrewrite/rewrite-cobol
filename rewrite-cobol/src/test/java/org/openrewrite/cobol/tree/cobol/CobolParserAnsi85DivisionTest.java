package org.openrewrite.cobol.tree.cobol;

import org.junit.jupiter.api.Test;
import org.openrewrite.cobol.CobolTest;

import static org.openrewrite.cobol.Assertions.cobol;

public class CobolParserAnsi85DivisionTest extends CobolTest {

    @Test
    void helloWorld() {
        rewriteRun(
          cobol(
            """
              000001 IDENTIFICATION  DIVISION .                                       C_AREA.01
              000002 PROGRAM-ID    . HELLO     .                                      C_AREA.02
              000003 PROCEDURE DIVISION.                                              C_AREA.03
              000004 DISPLAY 'Hello world!'.                                          C_AREA.04
              000005 STOP RUN.                                                        C_AREA.05
              """
          )
        );
    }

    @Test
    void arithmetic() {
        rewriteRun(
          cobol(
            """
              000001 IDENTIFICATION DIVISION .                                        C_AREA.01
              000002 PROGRAM-ID . HELLO-WORLD .                                       C_AREA.02
              000003 DATA DIVISION .                                                  C_AREA.03
              000004     WORKING-STORAGE SECTION .                                    C_AREA.04
              000005         77 X PIC 99.                                             C_AREA.05
              000006         77 Y PIC 99.                                             C_AREA.06
              000007         77 Z PIC 99.                                             C_AREA.07
              000008 PROCEDURE DIVISION .                                             C_AREA.08
              000009     SET X TO 10 .                                                C_AREA.09
              000010     SET Y TO 25 .                                                C_AREA.10
              000011     ADD X Y GIVING Z .                                           C_AREA.11
              000012     DISPLAY "X + Y = "Z .                                        C_AREA.12
              000013 STOP RUN .                                                       C_AREA.13
              """
          )
        );
    }

    @Test
    void environmentDivision() {
        rewriteRun(
          cobol(
            """
              000001 IDENTIFICATION DIVISION.                                         C_AREA.01
              000002 PROGRAM-ID.                                                      C_AREA.02
              000003     IC109A.                                                      C_AREA.03
              000004 ENVIRONMENT DIVISION.                                            C_AREA.04
              000005 CONFIGURATION SECTION.                                           C_AREA.05
              000006 SOURCE-COMPUTER.                                                 C_AREA.06
              000007     XXXXX082.                                                    C_AREA.07
              000008 OBJECT-COMPUTER.                                                 C_AREA.08
              000009     XXXXX083                                                     C_AREA.09
              000010     MEMORY SIZE XXXXX068 CHARACTERS                              C_AREA.10
              000011     PROGRAM COLLATING SEQUENCE IS COLLATING-SEQ-1.               C_AREA.11
              000012 SPECIAL-NAMES.                                                   C_AREA.12
              000013     ALPHABET PRG-COLL-SEQ IS                                     C_AREA.13
              000014     STANDARD-2.                                                  C_AREA.14
              000015 INPUT-OUTPUT SECTION.                                            C_AREA.15
              000016 FILE-CONTROL. SELECT OPTIONAL IDENTIFIER ASSIGN TO DISK.         C_AREA.16
              000017 I-O-CONTROL. IDENTIFIER.                                         C_AREA.17
              000018 RERUN ON IDENTIFIER EVERY 10 RECORDS                             C_AREA.18
              000019 SAME RECORD AREA FOR IDENTIFIER                                  C_AREA.19
              000020 MULTIPLE FILE TAPE CONTAINS IDENTIFIER POSITION 10               C_AREA.20
              000021 COMMITMENT CONTROL FOR IDENTIFIER.                               C_AREA.21
              """
          )
        );
    }

    @Test
    void inputOutputSection() {
        rewriteRun(
          cobol(
            """
              000001 IDENTIFICATION DIVISION.                                         C_AREA.01
              000002 PROGRAM-ID.                                                      C_AREA.02
              000003     IC109A.                                                      C_AREA.03
              000004 ENVIRONMENT DIVISION.                                            C_AREA.04
              000005 INPUT-OUTPUT SECTION.                                            C_AREA.05
              000006 FILE-CONTROL.                                                    C_AREA.06
              000007     SELECT PRINT-FILE ASSIGN TO                                  C_AREA.07
              000008         XXXXX055.                                                C_AREA.08
              000009     SELECT SEQ-FILE ASSIGN TO                                    C_AREA.09
              000010         XXXXX014.                                                C_AREA.10
              000011     SELECT SEQ-FILE RESERVE NO ALTERNATE AREA.                   C_AREA.11
              000012     SELECT SEQ-FILE ORGANIZATION IS RECORD BINARY INDEXED.       C_AREA.12
              000013     SELECT SEQ-FILE PADDING CHARACTER IS IDENTIFIER              C_AREA.13
              000014         IN IDENTIFIER.                                           C_AREA.14
              000015     SELECT SEQ-FILE RECORD DELIMITER IS STANDAR-1.               C_AREA.15
              000016     SELECT SEQ-FILE ACCESS MODE IS SEQUENTIAL.                   C_AREA.16
              000017     SELECT SEQ-FILE RECORD KEY IS IDENTIFIER IN IDENTIFIER       C_AREA.18
              000018         PASSWORD IS IDENTIFIER WITH DUPLICATES.                  C_AREA.18
              000019     SELECT SEQ-FILE ALTERNATE RECORD KEY IS IDENTIFIER IN        C_AREA.19
              000020         IDENTIFIER PASSWORD IS IDENTIFIER WITH DUPLICATES.       C_AREA.20
              000021     SELECT SEQ-FILE FILE STATUS IS IDENTIFIER IN IDENTIFIER      C_AREA.21
              000022         IDENTIFIER IN IDENTIFIER.                                C_AREA.22
              000023     SELECT SEQ-FILE RELATIVE KEY IS IDENTIFIER IN IDENTIFIER.    C_AREA.23
              """
          )
        );
    }

    @Test
    void procedureDivision() {
        rewriteRun(
          cobol(
            """
              000001 IDENTIFICATION  DIVISION .                                       C_AREA.01
              000002 PROGRAM-ID    . HELLO     .                                      C_AREA.02
              000003 PROCEDURE DIVISION USING GRP-01 GIVING dataName.                 C_AREA.03
              000004 DECLARATIVES.                                                    C_AREA.04
              000005 sectionName SECTION 77.                                          C_AREA.05
              000006 USE GLOBAL AFTER STANDARD ERROR PROCEDURE ON INPUT.              C_AREA.06
              000007 END DECLARATIVES.                                                C_AREA.07
              """
          )
        );
    }

    @Test
    void divisionUsing() {
        rewriteRun(
          cobol(
            """
              000001 IDENTIFICATION  DIVISION .                                       C_AREA.01
              000002 PROGRAM-ID    . HELLO     .                                      C_AREA.02
              000003 PROCEDURE DIVISION USING GRP-01.                                 C_AREA.03
              000004 STOP RUN.                                                        C_AREA.04
              """
          )
        );
    }

    @Test
    void ic109a() {
        rewriteRun(
          cobol(
            """
              000001 IDENTIFICATION DIVISION.                                         C_AREA.01
              000002 PROGRAM-ID.                                                      C_AREA.02
              000003     IC109A.                                                      C_AREA.03
              000004 ENVIRONMENT DIVISION.                                            C_AREA.04
              000005 CONFIGURATION SECTION.                                           C_AREA.05
              000006 SOURCE-COMPUTER.                                                 C_AREA.06
              000007     XXXXX082.                                                    C_AREA.07
              000008 OBJECT-COMPUTER.                                                 C_AREA.08
              000009     XXXXX083.                                                    C_AREA.09
              000010 INPUT-OUTPUT SECTION.                                            C_AREA.10
              000011 FILE-CONTROL.                                                    C_AREA.11
              000012     SELECT PRINT-FILE ASSIGN TO                                  C_AREA.12
              000013     XXXXX055.                                                    C_AREA.13
              000014 DATA DIVISION.                                                   C_AREA.14
              000015 FILE SECTION.                                                    C_AREA.15
              000016 FD  PRINT-FILE.                                                  C_AREA.16
              000017 01  PRINT-REC PICTURE X(120).                                    C_AREA.17
              000018 01  DUMMY-RECORD PICTURE X(120).                                 C_AREA.18
              000019 WORKING-STORAGE SECTION.                                         C_AREA.19
              000020 77  WS1 PICTURE X.                                               C_AREA.20
              000021 LINKAGE SECTION.                                                 C_AREA.21
              000022 01  GRP-01.                                                      C_AREA.22
              000023     02  SUB-CALLED.                                              C_AREA.23
              000024         03  DN1  PICTURE X(6).                                   C_AREA.24
              000025         03  DN2  PICTURE X(6).                                   C_AREA.25
              000026         03  DN3  PICTURE X(6).                                   C_AREA.26
              000027     02  TIMES-CALLED.                                            C_AREA.27
              000028         03  DN4  PICTURE S999.                                   C_AREA.28
              000029         03  DN5  PICTURE S999.                                   C_AREA.29
              000030         03  DN6  PICTURE S999.                                   C_AREA.30
              000031     02  SPECIAL-FLAGS.                                           C_AREA.31
              000032         03  DN7 PICTURE X.                                       C_AREA.32
              000033         03  DN8 PICTURE X.                                       C_AREA.33
              000034         03  DN9 PICTURE X.                                       C_AREA.34
              000035 PROCEDURE DIVISION USING GRP-01.                                 C_AREA.35
              000036 SECT-IC109-0001 SECTION.                                         C_AREA.36
              000037 PARA-IC109.                                                      C_AREA.37
              000038     MOVE "IC109A" TO DN1.                                        C_AREA.38
              000039     MOVE SPACE TO WS1.                                           C_AREA.39
              000040     CALL "IC110A" USING WS1 GRP-01.                              C_AREA.40
              000041     ADD 1 TO DN4.                                                C_AREA.41
              000042     MOVE WS1 TO DN9.                                             C_AREA.42
              000043 EXIT-IC109.                                                      C_AREA.43
              000045     EXIT PROGRAM.                                                C_AREA.45
              000046 END-OF                                                           C_AREA.46
              """
          )
        );
    }

    @Test
    void moveStatement() {
        rewriteRun(
          cobol(
            """
              000001 IDENTIFICATION DIVISION.                                         C_AREA.1
              000002 PROGRAM-ID. MOVETEST.                                            C_AREA.2
              000003 DATA DIVISION.                                                   C_AREA.3
              000004 PROCEDURE DIVISION USING GRP-01.                                 C_AREA.4
              000005 PARA-MOVETEST.                                                   C_AREA.5
              000006     MOVE "MOVETEST" TO DN1.                                      C_AREA.6
              000007     MOVE SPACE TO WS1.                                           C_AREA.7
              """
          )
        );
    }

    @Test
    void mergeStatement() {
        rewriteRun(
          cobol(
            """
              000001 IDENTIFICATION DIVISION.                                         C_AREA.1
              000002 PROGRAM-ID. MERGETEST.                                           C_AREA.2
              000003 PROCEDURE DIVISION.                                              C_AREA.3
              000004 MERGE-TEST.                                                      C_AREA.4
              000005     MERGE ST-FS4  ON ASCENDING KEY SORT-KEY                      C_AREA.5
              000006         USING  SQ-FS1  SQ-FS2                                    C_AREA.6
              000007         OUTPUT PROCEDURE IS MERGE-OUTPUT-PROC.                   C_AREA.7
              """
          )
        );
    }

    @Test
    void multiplyStatement() {
        rewriteRun(
          cobol(
            """
              000001 IDENTIFICATION DIVISION.                                         C_AREA.1
              000002 PROGRAM-ID. MULTIPLYTEST.                                        C_AREA.2
              000003 PROCEDURE DIVISION.                                              C_AREA.3
              000004 MULTIPLY -1.3 BY MULT4 ROUNDED.                                  C_AREA.4
              """
          )
        );
    }

    @Test
    void openStatement() {
        rewriteRun(
          cobol(
            """
              000001 IDENTIFICATION DIVISION.                                         C_AREA.1
              000002 PROGRAM-ID. OPENTEST.                                            C_AREA.2
              000003 PROCEDURE DIVISION.                                              C_AREA.3
              000004 OPEN OUTPUT SQ-FS2.                                              C_AREA.4
              000005 OPEN INPUT TFIL REVERSED.                                        C_AREA.5
              000006 OPEN INPUT TFIL WITH NO REWIND.                                  C_AREA.6
              """
          )
        );
    }

    @Test
    void performStatement() {
        rewriteRun(
          cobol(
            """
              000001 IDENTIFICATION DIVISION.                                         C_AREA.1
              000002 PROGRAM-ID. PARSERTEST.                                          C_AREA.2
              000003 PROCEDURE DIVISION.                                              C_AREA.3
              000004 PERFORM ST301M-MERGE THRU ST301M-SORT 1 TIMES.                   C_AREA.4
              """
          )
        );
    }

    @Test
    void readStatement() {
        rewriteRun(
          cobol(
            """
              000001 IDENTIFICATION DIVISION.                                         C_AREA.1
              000002 PROGRAM-ID. READTEST.                                            C_AREA.2
              000003 PROCEDURE DIVISION.                                              C_AREA.3
              000004 READ SQ-FS3 END .                                                C_AREA.4
              """
          )
        );
    }

    @Test
    void receiveStatement() {
        rewriteRun(
          cobol(
            """
              000001 IDENTIFICATION DIVISION.                                         C_AREA.1
              000002 PROGRAM-ID. MERGETEST.                                           C_AREA.2
              000003 PROCEDURE DIVISION.                                              C_AREA.3
              000004 RECEIVE CM-INQUE-1 MESSAGE INTO MSG-72                           C_AREA.4
              000005     NO DATA.                                                     C_AREA.5
              """
          )
        );
    }
}
