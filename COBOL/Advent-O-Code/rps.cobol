       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADVENT-RPS.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CHOICE.
           03 USER-CHOICE PIC A(1).
           03 COMP-CHOICE PIC A(1).
       01 WIN.
           03 COMP-WIN PIC 9 VALUE 0.
           03 USER-WIN PIC 9 VALUE 0.
       01 RAND.
           03 RAND-VAL PIC 9.
           03 SEED-STRING PIC X(16).
           03 SEED-NUM PIC 9(16) VALUE ZEROS.
       PROCEDURE DIVISION.
           DISPLAY "Choose R, P, or S"
           PERFORM UNTIL 1 = 0
      *>   ACCEPT USER INTPUT FROM TERMINAL AND CHECK FOR CORRECTNESS
               PERFORM USER-PIC
      *>   GENERATES RANDOM COMPUTER CHOICE  
               PERFORM COMP-PICK
      *>   CHECKS WHO WON THE ROUND AND IF SOMEONE WON THE GAME 
               PERFORM CHECK-WIN
           END-PERFORM.
           STOP RUN.
       USER-PIC.
           ACCEPT USER-CHOICE FROM CONSOLE.
           IF (USER-CHOICE NOT = "R") AND
              (USER-CHOICE NOT = "P") AND
              (USER-CHOICE NOT = "S")
              DISPLAY "Please choose a valid input (R, P, S)."
              PERFORM USER-PIC
           END-IF.
       COMP-PICK.
           PERFORM GEN-RAND.
           IF RAND-VAL = 0
               MOVE "R" TO COMP-CHOICE
           ELSE
               IF RAND-VAL = 1
                   MOVE "P" TO COMP-CHOICE
               ELSE
                   MOVE "S" TO COMP-CHOICE
               END-IF
           END-IF.
           DISPLAY "Computer chose: " COMP-CHOICE.
       GEN-RAND.
           MOVE FUNCTION CURRENT-DATE TO SEED-STRING.
           MOVE FUNCTION NUMVAL(SEED-STRING) TO SEED-NUM.
           COMPUTE RAND-VAL = FUNCTION RANDOM(SEED-NUM) * 3.
       CHECK-WIN.
           IF USER-CHOICE = COMP-CHOICE
               DISPLAY "It's a tie!"
               EXIT PARAGRAPH
           ELSE
               IF (USER-CHOICE = "R" AND COMP-CHOICE = "P") OR
                  (USER-CHOICE = "P" AND COMP-CHOICE = "S") OR
                  (USER-CHOICE = "S" AND COMP-CHOICE = "R")
                   DISPLAY "Computer beat you this round!"
                   COMPUTE COMP-WIN = COMP-WIN + 1
               ELSE
                   DISPLAY "You beat the computer this round!"
                   COMPUTE USER-WIN = USER-WIN + 1
           END-IF.

           IF USER-WIN = 2
               DISPLAY "You won!"
               STOP RUN.
           IF COMP-WIN = 2
               DISPLAY "Computer won!"
               STOP RUN.
       END PROGRAM ADVENT-RPS.