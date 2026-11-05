       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQ1.
       AUTHOR. DION VILLAGARCIA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EXERCISE
              ASSIGN TO "exercise.txt"
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  EXERCISE
           LABEL RECORD IS STANDARD
           DATA RECORD IS REC.
       01  REC            PIC X(40).

       WORKING-STORAGE SECTION.
       01 MORE        PIC X(3).
       01 MORE2       PIC X(40).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "INITIAL WORD"
           OPEN OUTPUT EXERCISE.
           MOVE "INITIAL WORLD" TO REC.
           WRITE REC.

           DISPLAY "DO YOU WANT TO ADD ANOTHER WORD?: "
           WITH NO ADVANCING
           ACCEPT MORE.

           PERFORM UNTIL MORE = "NO"
              IF MORE = "YES"
                 DISPLAY "INPUT ANOTHER WORD: " WITH NO ADVANCING
                 ACCEPT MORE2
                 MOVE MORE2 TO REC
                 WRITE REC
              ELSE
                 DISPLAY "PLEASE ANSWER YES OR NO."
              END-IF

              DISPLAY "DO YOU WANT TO ADD ANOTHER WORD?: "
                 WITH NO ADVANCING
              ACCEPT MORE
           END-PERFORM

           CLOSE EXERCISE.
           STOP RUN. 
