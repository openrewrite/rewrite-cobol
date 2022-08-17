/*
 * Copyright 2022 the original author or authors.
 * <p>
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * <p>
 * https://www.apache.org/licenses/LICENSE-2.0
 * <p>
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.openrewrite.cobol.tree

import org.junit.jupiter.api.Disabled
import org.junit.jupiter.api.Test
import org.openrewrite.ExecutionContext
import org.openrewrite.cobol.Assertions.cobol
import org.openrewrite.cobol.CobolVisitor
import org.openrewrite.test.RecipeSpec
import org.openrewrite.test.RewriteTest
import org.openrewrite.test.RewriteTest.toRecipe

//@Disabled("fix source before")
class CobolAnsi85DivisionTest : RewriteTest {

    override fun defaults(spec: RecipeSpec) {
        spec.recipe(toRecipe {
            object : CobolVisitor<ExecutionContext>() {
                override fun visitSpace(space: Space, p: ExecutionContext): Space {
                    if (space.whitespace.trim().isNotEmpty()) {
                        return space.withWhitespace("(~~>${space.whitespace}<~~)")
                    }
                    return space
                }
            }
        })
    }

    @Test
    fun helloWorld() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION  DIVISION .                                       C_AREA.01
            000002 PROGRAM-ID    . HELLO     .                                      C_AREA.02
            000003 PROCEDURE DIVISION.                                              C_AREA.03
            000004 DISPLAY 'Hello world!'.                                          C_AREA.04
            000005 STOP RUN.                                                        C_AREA.05
        """)
    )

    @Test
    fun arithmetic() = rewriteRun(
        cobol("""
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
        """)
    )

    @Test
    fun environmentDivision() = rewriteRun(
        cobol("""
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
        """)
    )

    @Test
    fun inputOutputSection() = rewriteRun(
        cobol("""
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
        """)
    )

    @Test
    fun procedureDivision() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION  DIVISION .                                       C_AREA.01
            000002 PROGRAM-ID    . HELLO     .                                      C_AREA.02
            000003 PROCEDURE DIVISION USING GRP-01 GIVING dataName.                 C_AREA.03
            000004 DECLARATIVES.                                                    C_AREA.04
            000005 sectionName SECTION 77.                                          C_AREA.05
            000006 USE GLOBAL AFTER STANDARD ERROR PROCEDURE ON INPUT.              C_AREA.06
            000007 END DECLARATIVES.                                                C_AREA.07
        """)
    )

    @Test
    fun divisionUsing() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION  DIVISION .                                       C_AREA.01
            000002 PROGRAM-ID    . HELLO     .                                      C_AREA.02
            000003 PROCEDURE DIVISION USING GRP-01.                                 C_AREA.03
            000004 STOP RUN.                                                        C_AREA.04
        """)
    )

    @Test
    fun ic109a() = rewriteRun(
        cobol("""
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
    )

    @Test
    fun moveStatement() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION DIVISION.                                         C_AREA.1
            000002 PROGRAM-ID. MOVETEST.                                            C_AREA.2
            000003 DATA DIVISION.                                                   C_AREA.3
            000004 PROCEDURE DIVISION USING GRP-01.                                 C_AREA.4
            000005 PARA-MOVETEST.                                                   C_AREA.5
            000006     MOVE "MOVETEST" TO DN1.                                      C_AREA.6
            000007     MOVE SPACE TO WS1.                                           C_AREA.7
        """)
    )

    @Test
    fun mergeStatement() = rewriteRun(
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
    )

    @Test
    fun multiplyStatement() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION DIVISION.                                         C_AREA.1
            000002 PROGRAM-ID. MULTIPLYTEST.                                        C_AREA.2
            000003 PROCEDURE DIVISION.                                              C_AREA.3
            000004 MULTIPLY -1.3 BY MULT4 ROUNDED.                                  C_AREA.4
        """)
    )

    @Test
    fun openStatement() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION DIVISION.                                         C_AREA.1
            000002 PROGRAM-ID. OPENTEST.                                            C_AREA.2
            000003 PROCEDURE DIVISION.                                              C_AREA.3
            000004 OPEN OUTPUT SQ-FS2.                                              C_AREA.4
            000005 OPEN INPUT TFIL REVERSED.                                        C_AREA.5
            000006 OPEN INPUT TFIL WITH NO REWIND.                                  C_AREA.6
        """)
    )

    @Test
    fun performStatement() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION DIVISION.                                         C_AREA.1
            000002 PROGRAM-ID. PARSERTEST.                                          C_AREA.2
            000003 PROCEDURE DIVISION.                                              C_AREA.3
            000004 PERFORM ST301M-MERGE THRU ST301M-SORT 1 TIMES.                   C_AREA.4
        """)
    )

    @Test
    fun readStatement() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION DIVISION.                                         C_AREA.1
            000002 PROGRAM-ID. READTEST.                                            C_AREA.2
            000003 PROCEDURE DIVISION.                                              C_AREA.3
            000004 READ SQ-FS3 END .                                                C_AREA.4
        """)
    )

    @Test
    fun receiveStatement() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION DIVISION.                                         C_AREA.1
            000002 PROGRAM-ID. MERGETEST.                                           C_AREA.2
            000003 PROCEDURE DIVISION.                                              C_AREA.3
            000004 RECEIVE CM-INQUE-1 MESSAGE INTO MSG-72                           C_AREA.4
            000005     NO DATA.                                                     C_AREA.5
        """)
    )

    @Test
    fun fileSection() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION DIVISION.                                         C_AREA.01
            000002 PROGRAM-ID.                                                      C_AREA.02
            000003     IC109A.                                                      C_AREA.03
            000004 DATA DIVISION.                                                   C_AREA.04
            000005 FILE SECTION.                                                    C_AREA.05
            000006 FD  PRINT-FILE.                                                  C_AREA.06
            000007 IS EXTERNAL.                                                     C_AREA.07
            000008 IS GLOBAL.                                                       C_AREA.08
            000009 BLOCK CONTAINS 1 TO 10 RECORDS.                                  C_AREA.09
            000010 RECORD CONTAINS 10 CHARACTERS.                                   C_AREA.10
            000011 RECORD IS VARYING IN SIZE FROM 1 TO 10 CHARACTERS                C_AREA.11
            000012     DEPENDING ON IDENTIFIER IN IDENTIFIER.                       C_AREA.12
            000013 RECORD CONTAINS 1 TO 10 CHARACTERS.                              C_AREA.13
            000014 LABEL RECORD IS OMITTED.                                         C_AREA.14
            000015 VALUE OF IDENTIFIER IS 10.                                       C_AREA.15
            000016 LINAGE IS 10 LINES WITH FOOTING AT 10.                           C_AREA.16
            000017 LINAGE IS 10 LINES AT TOP 10.                                    C_AREA.17
            000018 LINAGE IS 10 LINES AT BOTTOM 10.                                 C_AREA.18
            000019 CODE-SET IS IDENTIFIER.                                          C_AREA.19
            000020 RECORDING MODE IS IDENTIFIER.                                    C_AREA.20
            000021 DATA RECORD IS IDENTIFIER.                                       C_AREA.21
            000022 REPORT IS IDENTIFIER.                                            C_AREA.22
            000023 01  PRINT-REC PICTURE X(120).                                    C_AREA.23
            000024 01  DUMMY-RECORD PICTURE X(120).                                 C_AREA.24
        """)
    )

    @Test
    fun linkageSection() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION DIVISION.                                         C_AREA.01
            000002     PROGRAM-ID.                                                  C_AREA.02
            000003         IC109A.                                                  C_AREA.03
            000004     DATA DIVISION.                                               C_AREA.04
            000005     LINKAGE SECTION.                                             C_AREA.05
            000006     01  GRP-01.                                                  C_AREA.06
            000007         02  SUB-CALLED.                                          C_AREA.07
            000008             03  DN1  PICTURE X(6).                               C_AREA.08
            000009             03  DN2  PICTURE X(6).                               C_AREA.09
            000010             03  DN3  PICTURE X(6).                               C_AREA.10
            000011         02  TIMES-CALLED.                                        C_AREA.11
            000012             03  DN4  PICTURE S999.                               C_AREA.12
            000013             03  DN5  PICTURE S999.                               C_AREA.13
            000014             03  DN6  PICTURE S999.                               C_AREA.14
            000015         02  SPECIAL-FLAGS.                                       C_AREA.15
            000016             03  DN7 PICTURE X.                                   C_AREA.16
            000017             03  DN8 PICTURE X.                                   C_AREA.17
            000018             03  DN9 PICTURE X.                                   C_AREA.18
        """)
    )

    @Test
    fun localStorageSection() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION DIVISION.                                         C_AREA.01
            000002 PROGRAM-ID. LocalStorage.                                        C_AREA.02
            000003 DATA DIVISION.                                                   C_AREA.03
            000004 LOCAL-STORAGE Section.                                           C_AREA.04
            000005 01  NUM  PIC 9(4).                                               C_AREA.05
        """)
    )

    @Test
    fun dataBaseSection() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION DIVISION.                                         C_AREA.01
            000002 PROGRAM-ID. DBSection.                                           C_AREA.02
            000003 DATA DIVISION.                                                   C_AREA.03
            000004 DATA-BASE SECTION.                                               C_AREA.04
            000005 01 TRUE INVOKE TRUE                                              C_AREA.05
        """)
    )

    @Test
    fun screenSection() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION DIVISION.                                         C_AREA.01
            000002 PROGRAM-ID. DBSection.                                           C_AREA.02
            000003 DATA DIVISION.                                                   C_AREA.03
            000004 SCREEN SECTION.                                                  C_AREA.04
            000005 01 SCREEN1 BLANK LINE                                            C_AREA.05
            000006 BELL                                                             C_AREA.06
            000007 BLINK                                                            C_AREA.07
            000008 ERASE EOL                                                        C_AREA.08
            000009 HIGHLIGHT                                                        C_AREA.09
            000010 GRID                                                             C_AREA.10
            000011 UNDERLINE                                                        C_AREA.11
            000012 SIZE IS IDENTIFIER IN IDENTIFIER                                 C_AREA.12
            000013 LINE NUMBER IS PLUS IDENTIFIER IN IDENTIFIER                     C_AREA.13
            000014 COLUMN NUMBER IS PLUS IDENTIFIER IN IDENTIFIER                   C_AREA.14
            000015 FOREGROUND-COLOR IS IDENTIFIER IN IDENTIFIER                     C_AREA.15
            000016 BACKGROUND-COLOR IS IDENTIFIER IN IDENTIFIER                     C_AREA.16
            000017 CONTROL IS IDENTIFIER IN IDENTIFIER                              C_AREA.17
            000018 VALUE IS 10                                                      C_AREA.18
            000019 PICTURE IS $(10)                                                 C_AREA.19
            000020 FROM IDENTIFIER IN IDENTIFIER TO IDENTIFIER IN IDENTIFIER        C_AREA.20
            000021 USING IDENTIFIER IN IDENTIFIER                                   C_AREA.21
            000022 USAGE IS DISPLAY                                                 C_AREA.22
            000023 BLANK WHEN ZERO                                                  C_AREA.23
            000024 JUSTIFIED RIGHT                                                  C_AREA.24
            000025 SIGN IS LEADING SEPARATE CHARACTER                               C_AREA.25
            000026 AUTO                                                             C_AREA.26
            000027 SECURE                                                           C_AREA.27
            000028 REQUIRED                                                         C_AREA.28
            000029 PROMPT CHARACTER IS IDENTIFIER IN IDENTIFIER OCCURS 01 TIMES     C_AREA.29
            000030 FULL                                                             C_AREA.30
            000031 ZERO-FILL                                                        C_AREA.31
            000032 .                                                                C_AREA.32
        """)
    )

    @Disabled("Potential lexer issue: The REVERSE-VIDEO token maps to RESERVE-VIDEO")
    @Test
    fun reverseVideo() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION DIVISION.                                         C_AREA.01
            000002 PROGRAM-ID. DBSection.                                           C_AREA.02
            000003 DATA DIVISION.                                                   C_AREA.03
            000004 SCREEN SECTION.                                                  C_AREA.04
            000005 01 REVERSE-VIDEO.                                                C_AREA.05
        """)
    )

    @Test
    fun acceptStatement() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION DIVISION.                                         C_AREA.01
            000002 PROGRAM-ID. acceptStatement.                                     C_AREA.02
            000003 PROCEDURE DIVISION.                                              C_AREA.03
            000004 PARAGRAPH_NAME.                                                  C_AREA.04
            000005 ACCEPT identifier FROM DATE YYYYMMDD END-ACCEPT                  C_AREA.05
            000006 ACCEPT identifier FROM ESCAPE KEY                                C_AREA.06
            000007 ACCEPT identifier FROM mnemonicName                              C_AREA.07
            000008 ACCEPT identifier MESSAGE COUNT.                                 C_AREA.08
        """)
    )

    @Test
    fun alterStatement() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION DIVISION .                                        C_AREA.01
            000002 PROGRAM-ID . HELLO-WORLD .                                       C_AREA.02
            000003 PROCEDURE DIVISION .                                             C_AREA.03
            000004 ALTER PARA-54 TO PROCEED TO PARA-54B.                            C_AREA.04
            000005 ALTER PARA-23 TO PARA-24.                                        C_AREA.05
        """)
    )

    @Test
    fun cancelStatement() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION DIVISION.                                         C_AREA.01
            000002 PROGRAM-ID. acceptStatement.                                     C_AREA.02
            000003 PROCEDURE DIVISION.                                              C_AREA.03
            000004 PARAGRAPH_NAME.                                                  C_AREA.04
            000005 CANCEL "literal"                                                 C_AREA.05
            000006 CANCEL identifier                                                C_AREA.06
            000007 CANCEL libraryName BYTITLE.                                      C_AREA.07
        """)
    )

    @Test
    fun closeStatement() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION DIVISION.                                         C_AREA.01
            000002 PROGRAM-ID. acceptStatement.                                     C_AREA.02
            000003 PROCEDURE DIVISION.                                              C_AREA.03
            000004 PARAGRAPH_NAME.                                                  C_AREA.04
            000005 CLOSE fileName UNIT FOR REMOVAL WITH LOCK                        C_AREA.05
            000006 CLOSE fileName WITH NO REWIND                                    C_AREA.06
            000007 CLOSE fileName NO WAIT USING CLOSE-DISPOSITION OF ABORT          C_AREA.07
            000008 CLOSE fileName NO WAIT USING ASSOCIATED-DATA identifier          C_AREA.08
            000009 CLOSE fileName NO WAIT USING ASSOCIATED-DATA-LENGTH              C_AREA.09
            000010 OF identifier.                                                   C_AREA.10
        """)
    )

    @Test
    fun rewriteStatement() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION DIVISION.                                         C_AREA.01
            000002 PROGRAM-ID. acceptStatement.                                     C_AREA.02
            000003 PROCEDURE DIVISION.                                              C_AREA.03
            000004 PARAGRAPH_NAME.                                                  C_AREA.04
            000005 REWRITE dataName IN fileName END-REWRITE.                        C_AREA.05
        """)
    )

    @Test
    fun callStatement() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION DIVISION.                                         C_AREA.01
            000002 PROGRAM-ID. acceptStatement.                                     C_AREA.02
            000003 PROCEDURE DIVISION USING GRP-01.                                 C_AREA.03
            000004 SECT-IC109-0001 SECTION.                                         C_AREA.04
            000005 PARA-IC109.                                                      C_AREA.05
            000006     CALL "IC110A" USING BY REFERENCE WS1 GRP-01.                 C_AREA.06
            000007     CALL "IC110A" USING BY VALUE ADDRESS OF GRP-01.              C_AREA.07
            000008     CALL "IC110A" USING BY CONTENT LENGTH OF GRP-01.             C_AREA.08
            000009     CALL "IC110A" GIVING GRP-01.                                 C_AREA.09
            000010     CALL "IC110A" ON OVERFLOW CONTINUE.                          C_AREA.10
            000011     CALL "IC110A" ON EXCEPTION CONTINUE.                         C_AREA.11
            000012     CALL "IC110A" NOT ON EXCEPTION CONTINUE.                     C_AREA.12
        """)
    )

    @Test
    fun writeStatement() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION DIVISION.                                         C_AREA.01
            000002 PROGRAM-ID. acceptStatement.                                     C_AREA.02
            000003 PROCEDURE DIVISION USING GRP-01.                                 C_AREA.03
            000004 PARA-IC109.                                                      C_AREA.04
            000005     WRITE IC110A FROM GRP-01.                                    C_AREA.05
            000006     WRITE IC110A BEFORE ADVANCING PAGE.                          C_AREA.06
            000007     WRITE IC110A BEFORE ADVANCING 10 LINES.                      C_AREA.07
            000008     WRITE IC110A BEFORE ADVANCING GRP-01.                        C_AREA.08
            000009     WRITE IC110A AT END-OF-PAGE CONTINUE.                        C_AREA.09
            000010     WRITE IC110A NOT AT END-OF-PAGE CONTINUE.                    C_AREA.10
            000011     WRITE IC110A INVALID KEY CONTINUE.                           C_AREA.11
            000012     WRITE IC110A NOT INVALID KEY CONTINUE.                       C_AREA.12
        """)
    )

    @Test
    fun computeStatement() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION DIVISION .                                        C_AREA.01
            000002 PROGRAM-ID . HELLO-WORLD .                                       C_AREA.02
            000003 PROCEDURE DIVISION .                                             C_AREA.03
            000004     COMPUTE V = (1 + 2) .                                        C_AREA.04
            000005     COMPUTE LAG-TIME =                                           C_AREA.05
            000006         ((SYS-HRS * 3600) + (SYS-MINS * 60) + SYS-SECS) -        C_AREA.06
            000007         ((HOURS OF MSG-TIME * 3600) + (MINUTES OF MSG-TIME * 60) C_AREA.07
            000008         + SECONDS OF MSG-TIME)                                   C_AREA.08
            000009         END-COMPUTE .                                            C_AREA.09
        """)
    )

    @Test
    fun divideStatement() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION DIVISION.                                         C_AREA.01
            000002 PROGRAM-ID. acceptStatement.                                     C_AREA.02
            000003 PROCEDURE DIVISION USING GRP-01.                                 C_AREA.03
            000004 SIG-TEST-GF-5-0.                                                 C_AREA.04
            000005     DIVIDE 0.097 INTO DIV7 ROUNDED.                              C_AREA.05
            000006     DIVIDE 0.097 INTO DIV7 GIVING DIV8 ROUNDED.                  C_AREA.06
            000007     DIVIDE 0.097 BY DIV7 GIVING DIV8 ROUNDED.                    C_AREA.07
            000008     DIVIDE 0.097 INTO DIV7 REMAINDER DIV9.                       C_AREA.08
            000009     DIVIDE 0.097 INTO DIV7 ON SIZE ERROR CONTINUE.               C_AREA.09
            000010     DIVIDE 0.097 INTO DIV7 NOT ON SIZE ERROR CONTINUE.           C_AREA.10
        """)
    )

    @Test
    fun evaluateStatement() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION DIVISION.                                         C_AREA.01
            000002 PROGRAM-ID. acceptStatement.                                     C_AREA.02
            000003 PROCEDURE DIVISION USING GRP-01.                                 C_AREA.03
            000004 F-ANNUITY-02.                                                    C_AREA.04
            000005 EVALUATE IC110A END-EVALUATE.                                    C_AREA.05
            000006 EVALUATE IC110A ALSO IC110B.                                     C_AREA.06
            000007 EVALUATE IC110A                                                  C_AREA.07
            000008 WHEN ANY ALSO ANY                                                C_AREA.08
            000009     CONTINUE                                                     C_AREA.09
            000010 WHEN IDENTIFIER THRU IDENTIFIER                                  C_AREA.10
            000011     CONTINUE                                                     C_AREA.11
            000012 WHEN TRUE                                                        C_AREA.12
            000013     CONTINUE                                                     C_AREA.13
            000014 WHEN OTHER                                                       C_AREA.14
            000015     CONTINUE.                                                    C_AREA.15
        """)
    )

    @Test
    fun conditions() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION DIVISION.                                         C_AREA.01
            000002 PROGRAM-ID. acceptStatement.                                     C_AREA.02
            000003 PROCEDURE DIVISION USING GRP-01.                                 C_AREA.03
            000004 F-ANNUITY-02.                                                    C_AREA.04
            000005 EVALUATE IC110A                                                  C_AREA.05
            000006 WHEN IDENTIFIER IS NOT ALPHABETIC-LOWER                          C_AREA.06
            000007     CONTINUE                                                     C_AREA.07
            000008 WHEN IDENTIFIER IN IDENTIFIER                                    C_AREA.08
            000009     CONTINUE.                                                    C_AREA.09
        """)
    )

    @Test
    fun conditionNameSubscriptReference() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION DIVISION.                                         C_AREA.01
            000002 PROGRAM-ID. acceptStatement.                                     C_AREA.02
            000003 PROCEDURE DIVISION.                                              C_AREA.03
            000004 COMMA-SUBSCRIPT-TEST.                                            C_AREA.04
            000005 EVALUATE NOT IDENTIFIER (IDENTIFIER, IDENTIFIER IDENTIFIER)      C_AREA.05
            000006 .                                                                C_AREA.06
        """)
    )

    @Test
    fun sendStatement() = rewriteRun(
        cobol("""
            000001 IDENTIFICATION DIVISION.                                         C_AREA.01
            000002 PROGRAM-ID. acceptStatement.                                     C_AREA.02
            000003 PROCEDURE DIVISION.                                              C_AREA.03
            000004 SEND CM-OUTQUE-1 FROM MSG-70 WITH EMI                            C_AREA.04
            000005     AFTER ADVANCING PAGE.                                        C_AREA.05
        """)
    )

    @Test
    fun tableCallTest() = rewriteRun(
        cobol(
            """
            IDENTIFICATION DIVISION.
            PROGRAM-ID. acceptStatement.
            PROCEDURE DIVISION.
            COMMA-SUBSCRIPT-TEST.
            EVALUATE SUBSCRIPT
            WHEN IDENTIFIER (IDENTIFIER, IDENTIFIER IDENTIFIER)
                CONTINUE.
        """
        )
    )

    @Test
    fun functionCallTest() = rewriteRun(
        cobol(
            """
            IDENTIFICATION DIVISION.
            PROGRAM-ID. acceptStatement.
            PROCEDURE DIVISION.
            COMMA-SUBSCRIPT-TEST.
            EVALUATE SUBSCRIPT
            WHEN IDENTIFIER (FUNCTION INTEGER (IDENTIFIER, IDENTIFIER IDENTIFIER) (1: 10))
                CONTINUE.
        """
        )
    )

    @Test
    fun relationConditions() = rewriteRun(
        cobol(
            """
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
    )

    @Test
    fun multiElementLiteral() = rewriteRun(
        cobol(
            """
            IDENTIFICATION DIVISION.
            PROGRAM-ID. acceptStatement.
            PROCEDURE DIVISION.
            Literal-Test.
            EVALUATE DFHRESP (IDENTIFIER).
        """
        )
    )

    @Test
    fun multiElementIdentifier() = rewriteRun(
        cobol(
            """
            IDENTIFICATION DIVISION.
            PROGRAM-ID. acceptStatement.
            PROCEDURE DIVISION.
            Identifier-Test.
            EVALUATE IDENTIFIER IN IDENTIFIER.
        """
        )
    )

    @Test
    fun openMultipleStatements() = rewriteRun(
        cobol(
            """
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
    )

    @Test
    fun outOfOrderOpenStatements() = rewriteRun(
        cobol(
            """
            IDENTIFICATION DIVISION.
            PROGRAM-ID. acceptStatement.
            PROCEDURE DIVISION.
            OPEN-FILES.
                OPEN     INPUT IDENTIFIER OUTPUT IDENTIFIER INPUT IDENTIFIER OUTPUT IDENTIFIER.
        """
        )
    )

    @Test
    fun unstringStatement() = rewriteRun(
        cobol(
            """
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
    )

    @Test
    fun terminateStatement() = rewriteRun(
        cobol(
            """
            IDENTIFICATION DIVISION.
            PROGRAM-ID. terminateStatement.
            PROCEDURE DIVISION.
            RW301M-CONTROL.
                TERMINATE RFIL2.
        """
        )
    )

    @Test
    fun generateStatement() = rewriteRun(
        cobol(
            """
            IDENTIFICATION DIVISION.
            PROGRAM-ID. terminateStatement.
            PROCEDURE DIVISION.
            RW301M-CONTROL.
                GENERATE RREC.
        """
        )
    )

    @Test
    fun subtractStatement() = rewriteRun(
        cobol(
            """
            IDENTIFICATION DIVISION.
            PROGRAM-ID. subtractStatement.
            PROCEDURE DIVISION.
            RW301M-CONTROL.
                SUBTRACT 1 FROM ERROR-COUNTER
                SUBTRACT N-10 FROM 0 GIVING N-19
                SUBTRACT CORRESPONDING IDENTIFIER FROM IDENTIFIER ROUNDED.
        """
        )
    )

    @Test
    fun exitStatement() = rewriteRun(
        cobol(
            """
            IDENTIFICATION DIVISION.
            PROGRAM-ID. exitStatement.
            PROCEDURE DIVISION.
            RW301M-CONTROL.
                EXIT PROGRAM.
        """
        )
    )

    @Test
    fun sortStatement() = rewriteRun(
        cobol(
            """
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
    )

    @Test
    fun stringStatement() = rewriteRun(
        cobol(
            """
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
    )

    @Test
    fun startStatement() = rewriteRun(
        cobol(
            """
            IDENTIFICATION DIVISION.
            PROGRAM-ID. subtractStatement.
            PROCEDURE DIVISION.
            STA-TEST-GF-01.
                START IX-FS2 KEY IS NOT LESS THAN IDENTIFIER IN IDENTIFIER END-START.
            .
        """
        )
    )

    @Test
    fun goToStatement() = rewriteRun(
        cobol(
            """
            IDENTIFICATION DIVISION.
            PROGRAM-ID. subtractStatement.
            PROCEDURE DIVISION.
            ACCEPT-TEST-01.
                GO TO CM105-FINI.
                GO TO CM105-FINI DEPENDING ON IDENTIFIER IN IDENTIFIER.
            .
        """
        )
    )

    @Test
    fun ifStatement() = rewriteRun(
        cobol(
            """
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
    )

    @Test
    fun initializeStatement() = rewriteRun(
        cobol(
            """
            IDENTIFICATION DIVISION.
            PROGRAM-ID. subtractStatement.
            PROCEDURE DIVISION.
            INI-TEST-GF-1-0.
                INITIALIZE IDENTIFIER IN IDENTIFIER REPLACING NATIONAL DATA BY 42.
            .
        """
        )
    )

    @Test
    fun initiateStatement() = rewriteRun(
        cobol(
            """
            IDENTIFICATION DIVISION.
            PROGRAM-ID. subtractStatement.
            PROCEDURE DIVISION.
            RW301M-CONTROL.
                INITIATE RFIL2.
            .
        """
        )
    )

    @Test
    fun inspectStatement() = rewriteRun(
        cobol(
            """
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
    )

    @Test
    fun communicationSection() = rewriteRun(
        cobol(
            """
            IDENTIFICATION DIVISION.
            PROGRAM-ID. communicationSection.
            DATA DIVISION.
            COMMUNICATION SECTION.
                CD COMMNAME FOR INITIAL INPUT.
                CD COMMNAME FOR OUTPUT.
                CD COMMNAME FOR INITIAL I-O.
        """
        )
    )

    @Test
    fun reportSection() = rewriteRun(
        cobol(
            """
            IDENTIFICATION DIVISION.
            PROGRAM-ID. communicationSection.
            DATA DIVISION.
            REPORT SECTION.
                RD IDENTIFIER IN IDENTIFIER IS GLOBAL.
                10 IDENTIFIER LINE NUMBER IS 10 ON NEXT PAGE.
        """
        )
    )

    @Test
    fun programLibrarySection() = rewriteRun(
        cobol(
            """
            IDENTIFICATION DIVISION.
            PROGRAM-ID. communicationSection.
            DATA DIVISION.
            PROGRAM-LIBRARY SECTION.
                LD IDENTIFIER EXPORT ATTRIBUTE SHARING IS DONTCARE ENTRY-PROCEDURE IDENTIFIER FOR ZERO
                LB IDENTIFIER IMPORT IS GLOBAL IS COMMON ATTRIBUTE
                FUNCTIONNAME IS ZERO LIBACCESS IS BYFUNCTION LIBPARAMETER IS ZERO
        """
        )
    )
}
