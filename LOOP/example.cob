       IDENTIFICATION DIVISION.
       PROGRAM-ID. DION.
       AUTHOR. DION VILLAGARCIA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 N               PIC 999.
       01 M               PIC 999.
       01 ORIG-N          PIC 999.
       01 ORIG-M          PIC 999.
       01 WORK-N          PIC 999.
       01 WORK-M          PIC 999.
       01 SQUARE          PIC 9(5).
       01 CUBE            PIC 9(5).

       01 DISP-S          PIC ZZZZ9.
       01 DISP-C          PIC ZZZZ9.
       01 DISP-O-N        PIC ZZ9.
       01 DISP-O-M        PIC ZZ9.
       01 FACTORIAL       PIC 9(5) VALUE 1.
       01 COUNTER         PIC 99   VALUE 1.
       01 DISP-FAC        PIC ZZZZ9.
       01 FAC-M           PIC 9(5).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.   
           DISPLAY "Input the first number: " WITH NO ADVANCING
           ACCEPT N.
           DISPLAY "Input the second number: " WITH NO ADVANCING
           ACCEPT M.

           MOVE N TO WORK-N.
           MOVE M TO WORK-M.
           MOVE N TO ORIG-N.
           MOVE M TO ORIG-M.
           MOVE M TO FAC-M.

           COMPUTE SQUARE = WORK-N * WORK-N.
           MOVE SQUARE TO DISP-S.
           DISPLAY "The square of the number is: "FUNCTION TRIM(DISP-S).
           
           COMPUTE CUBE = WORK-M * WORK-M * WORK-M
           MOVE CUBE TO DISP-C
           DISPLAY "The cube of the number is: "FUNCTION TRIM(DISP-C).

           IF ORIG-N > ORIG-M
              MOVE ORIG-N TO DISP-O-N
              DISPLAY FUNCTION TRIM(DISP-O-N) " IS THE LARGEST NUMBER"
           ELSE
              MOVE ORIG-M TO DISP-O-M
              DISPLAY FUNCTION TRIM(DISP-O-M) " IS THE LARGEST NUMBER"
           END-IF.

           PERFORM UNTIL COUNTER > FAC-M
              COMPUTE FACTORIAL = FACTORIAL * COUNTER
              ADD 1 TO COUNTER
           END-PERFORM.

           MOVE FACTORIAL TO DISP-FAC.
           DISPLAY "THE FACTORIAL OF "FUNCTION TRIM(FAC-M) " IS "
           FUNCTION TRIM(DISP-FAC).

       STOP RUN.
