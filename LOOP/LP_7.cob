       IDENTIFICATION DIVISION.
       PROGRAM-ID. DIGIT_SUM.
       AUTHOR. DION VILLAGARCIA.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 NUMBER1   PIC 9(5).
       01 SUM1      PIC 99999 VALUE 0.
       01 DIGIT     PIC 9.

       01 DISP-SUM  PIC ZZZZ9.


       PROCEDURE DIVISION.

           DISPLAY "Input Number: " WITH NO ADVANCING
           ACCEPT NUMBER1.

           PERFORM UNTIL NUMBER1 <= 0
                   COMPUTE DIGIT = FUNCTION MOD(NUMBER1, 10)
                   COMPUTE SUM1 = SUM1 + DIGIT
                   COMPUTE NUMBER1 = NUMBER1 / 10
           END-PERFORM.
           
           MOVE SUM1 TO DISP-SUM
           DISPLAY "Digit sum is: " FUNCTION TRIM(DISP-SUM).

           STOP RUN.