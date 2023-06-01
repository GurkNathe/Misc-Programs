       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADVENT-CALORIES.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ADVENT
           ASSIGN TO "advent.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD ADVENT.
       01 CAL PIC 9(6).
       WORKING-STORAGE SECTION.
       01 CUR-CAL PIC 9(6) VALUE 0.
       01 MAX-CAL PIC 9(6) VALUE 0.
       01 ELF PIC 9 VALUE 1.
       01 MAX-ELF PIC 9 VALUE 1.
       01 WS-EOF PIC X(1) VALUE 'N'.
       01 BLANK-LINE PIC X(6) VALUE SPACES.
       PROCEDURE DIVISION.
           OPEN INPUT ADVENT
           PERFORM READ-PROCEDURE UNTIL WS-EOF = 'Y'
           CLOSE ADVENT
           DISPLAY MAX-CAL
           DISPLAY MAX-ELF
           STOP RUN.
       READ-PROCEDURE.
           READ ADVENT
           AT END MOVE 'Y' TO WS-EOF
           NOT AT END PERFORM CALC-PROCEDURE
           END-READ.
       CALC-PROCEDURE.
           IF CAL = BLANK-LINE
               MOVE ZEROS TO CUR-CAL
               COMPUTE ELF = ELF + 1
               EXIT PARAGRAPH
           END-IF.
           COMPUTE CUR-CAL = CUR-CAL + CAL
           IF CUR-CAL > MAX-CAL
               MOVE CUR-CAL TO MAX-CAL
               MOVE ELF TO MAX-ELF
           END-IF.
       END PROGRAM ADVENT-CALORIES.
           