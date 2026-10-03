       IDENTIFICATION DIVISION.
       PROGRAM-ID. ODD-EVEN.
       AUTHOR. DION VILLAGARCIA.
       DATE-WRITTEN. SEPT 10, 2026.
       DATE-COMPILED. SEPT 10, 2026.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 NUM1         PIC 9(5).

       01 DISP-NUMPUT  PIC ZZZZ9.

       PROCEDURE DIVISION.

           DISPLAY "Input Number: " WITH NO ADVANCING
           ACCEPT NUM1.

           MOVE NUM1 TO DISP-NUMPUT.

           IF NUM1 = 0 
              DISPLAY FUNCTION TRIM(DISP-NUMPUT) " is neutral"
           ELSE
              IF FUNCTION MOD(NUM1, 2) = 0
                 DISPLAY FUNCTION TRIM(DISP-NUMPUT) " is even"
              ELSE
                 DISPLAY FUNCTION TRIM(DISP-NUMPUT) " is odd"
              END-IF.

           STOP RUN.