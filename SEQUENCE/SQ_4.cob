       IDENTIFICATION DIVISION.
       PROGRAM-ID. F1_N4_VILLAGARCIA.
       AUTHOR. DION_VILLAGARCIA.
       DATE-WRITTEN. SEPT 03, 2026.
       DATE-COMPILED. SEPT 03, 2026.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 F        PIC 9(2)V99.
       01 CELSIUS  PIC 9(2)V99.

       PROCEDURE DIVISION.
           DISPLAY "Input Celsius: ".
           ACCEPT CELSIUS.
           COMPUTE F = CELSIUS * 9 / 5 + 32.

           DISPLAY "The converted Celsius of " CELSIUS " is " F.

           STOP RUN.