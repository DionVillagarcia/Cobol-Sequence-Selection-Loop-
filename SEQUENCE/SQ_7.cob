       IDENTIFICATION DIVISION.
       PROGRAM-ID. F1_N7_VILLAGARCIA.
       AUTHOR. DION_VILLAGARCIA.
       DATE-WRITTEN. SEPT 03, 2026.
       DATE-COMPILED. SEPT 03, 2026.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 SQUARED       PIC 9(5).
       01 CUBED         PIC 9(5).
       01 NUM           PIC 9(5).

       01 DISP-NUM      PIC ZZZZ9.
       01 DISP-SQUARED  PIC ZZZZ9.
       01 DISP-CUBED    PIC ZZZZ9.

       PROCEDURE DIVISION.
           DISPLAY "Input a number: "
           ACCEPT NUM.



           COMPUTE SQUARED = NUM * NUM.
           COMPUTE CUBED = NUM * NUM * NUM.

           MOVE NUM TO DISP-NUM.
           MOVE SQUARED TO DISP-SQUARED.
           MOVE CUBED TO DISP-CUBED.



           DISPLAY "The Squared of "
                   FUNCTION TRIM(DISP-NUM)
                   " is "
                   FUNCTION TRIM(DISP-SQUARED).
           DISPLAY "The Cubed   of "
                   FUNCTION TRIM(DISP-NUM)
                   " is "
                   FUNCTION TRIM(DISP-CUBED).



           STOP RUN.